// GetOptimum DP + the CodeOneBlock driving loop. Included into `encoder.rs`
// (shares the private `Encoder`). A faithful port of LzmaEnc.c:1219-1961 and
// :2400-2664.

impl<'a> Encoder<'a> {
    fn get_optimum(&mut self, position_in: u32) -> u32 {
        let mut position = position_in;
        self.opt_cur = 0;
        self.opt_end = 0;

        let main_len = if self.additional_offset == 0 {
            self.read_match_distances()
        } else {
            self.longest_match_len
        };
        let mut num_pairs = self.num_pairs;

        let mut num_avail = self.num_avail;
        if num_avail < 2 {
            self.back_res = MARK_LIT;
            return 1;
        }
        if num_avail > MATCH_LEN_MAX {
            num_avail = MATCH_LEN_MAX;
        }

        let data = position as usize;
        let mut reps = [0u32; 4];
        let mut rep_lens = [0u32; 4];
        let mut rep_max_index = 0usize;
        for i in 0..NUM_REPS {
            reps[i] = self.reps[i];
            let r = reps[i] as usize;
            if self.input[data] != self.input[data - r] || self.input[data + 1] != self.input[data + 1 - r]
            {
                rep_lens[i] = 0;
                continue;
            }
            let mut len = 2usize;
            while (len as u32) < num_avail && self.input[data + len] == self.input[data + len - r] {
                len += 1;
            }
            rep_lens[i] = len as u32;
            if len as u32 > rep_lens[rep_max_index] {
                rep_max_index = i;
            }
            if len as u32 == MATCH_LEN_MAX {
                break;
            }
        }

        if rep_lens[rep_max_index] >= self.num_fast_bytes {
            self.back_res = rep_max_index as u32;
            let len = rep_lens[rep_max_index];
            self.move_pos(len - 1);
            return len;
        }

        if main_len >= self.num_fast_bytes {
            self.back_res = self.matches[num_pairs as usize - 1] + NUM_REPS as u32;
            self.move_pos(main_len - 1);
            return main_len;
        }

        let cur_byte = self.input[data] as u32;
        let match_byte = self.input[data - reps[0] as usize] as u32;

        let mut last = rep_lens[rep_max_index];
        if last <= main_len {
            last = main_len;
        }
        if last < 2 && cur_byte != match_byte {
            self.back_res = MARK_LIT;
            return 1;
        }

        self.opt[0].state = self.state;
        let pos_state = (position & self.pb_mask) as usize;
        let state = self.state as usize;

        {
            let prev = self.input[data - 1] as u32;
            let base = self.lit_base(position, prev);
            let lit = if !is_lit_state(self.state) {
                lit_matched_get_price(&self.probs.literal[base..base + 0x300], cur_byte, match_byte, &self.prices.pp)
            } else {
                lit_get_price(&self.probs.literal[base..base + 0x300], cur_byte, &self.prices.pp)
            };
            self.opt[1].price = self.prices.pp.price_0(self.probs.is_match[state][pos_state]) + lit;
        }
        self.opt[1].dist = MARK_LIT;
        self.opt[1].extra = 0;

        let match_price = self.prices.pp.price_1(self.probs.is_match[state][pos_state]);
        let rep_match_price = match_price + self.prices.pp.price_1(self.probs.is_rep[state]);

        if match_byte == cur_byte && rep_lens[0] == 0 {
            let short_rep_price =
                rep_match_price + price_short_rep(&self.probs, &self.prices.pp, state, pos_state);
            if short_rep_price < self.opt[1].price {
                self.opt[1].price = short_rep_price;
                self.opt[1].dist = 0;
                self.opt[1].extra = 0;
            }
            if last < 2 {
                self.back_res = self.opt[1].dist;
                return 1;
            }
        }

        self.opt[1].len = 1;
        self.opt[0].reps = reps;

        // ---- REP seeding ----
        for i in 0..NUM_REPS {
            let mut rep_len = rep_lens[i];
            if rep_len < 2 {
                continue;
            }
            let price = rep_match_price + price_pure_rep(&self.probs, &self.prices.pp, i, state, pos_state);
            loop {
                let price2 = price + self.prices.rep_len_enc.price(pos_state, rep_len);
                if price2 < self.opt[rep_len as usize].price {
                    let o = &mut self.opt[rep_len as usize];
                    o.price = price2;
                    o.len = rep_len;
                    o.dist = i as u32;
                    o.extra = 0;
                }
                rep_len -= 1;
                if rep_len < 2 {
                    break;
                }
            }
        }

        // ---- MATCH seeding ----
        let mut len = rep_lens[0] + 1;
        if len <= main_len {
            let mut offs = 0usize;
            let normal_match_price = match_price + self.prices.pp.price_0(self.probs.is_rep[state]);
            if len < 2 {
                len = 2;
            } else {
                while len > self.matches[offs] {
                    offs += 2;
                }
            }
            loop {
                let dist = self.matches[offs + 1];
                let mut price = normal_match_price + self.prices.len_enc.price(pos_state, len);
                let ltp = len_to_pos_state(len);
                if dist < NUM_FULL_DISTANCES {
                    price += self.prices.distances_prices[ltp][(dist & (NUM_FULL_DISTANCES - 1)) as usize];
                } else {
                    let slot = optimum::get_pos_slot(dist) as usize;
                    price += self.prices.align_prices[(dist & ALIGN_MASK) as usize];
                    price += self.prices.pos_slot_prices[ltp][slot];
                }
                if price < self.opt[len as usize].price {
                    let o = &mut self.opt[len as usize];
                    o.price = price;
                    o.len = len;
                    o.dist = dist + NUM_REPS as u32;
                    o.extra = 0;
                }
                if len == self.matches[offs] {
                    offs += 2;
                    if offs as u32 == num_pairs {
                        break;
                    }
                }
                len += 1;
            }
        }

        let mut cur = 0usize;

        // ---- optimal parsing loop ----
        loop {
            cur += 1;
            if cur == last as usize {
                break;
            }

            if cur >= NUM_OPTS - 64 {
                let mut price = self.opt[cur].price;
                let mut best = cur;
                for j in (cur + 1)..=(last as usize) {
                    let price2 = self.opt[j].price;
                    if price >= price2 {
                        price = price2;
                        best = j;
                    }
                }
                let delta = (best - cur) as u32;
                self.move_pos(delta);
                cur = best;
                break;
            }

            let mut new_len = self.read_match_distances();
            num_pairs = self.num_pairs;
            if new_len >= self.num_fast_bytes {
                self.longest_match_len = new_len;
                break;
            }

            position += 1;
            let cur_len = self.opt[cur].len;
            let mut prev = cur - cur_len as usize;
            let state: u32;
            // `reps` persists across iterations: a len==1 step (literal / short rep)
            // leaves the recent distances unchanged, so it reuses the carried value
            // (mirrors the function-scoped `reps` in the C).

            if cur_len == 1 {
                let pstate = self.opt[prev].state as usize;
                state = if self.opt[cur].dist == 0 {
                    u32::from(SHORT_REP_NEXT_STATES[pstate])
                } else {
                    u32::from(LITERAL_NEXT_STATES[pstate])
                };
            } else {
                let dist = self.opt[cur].dist;
                let extra = self.opt[cur].extra;
                if extra != 0 {
                    prev -= extra as usize;
                    state = if extra == 1 {
                        if dist < NUM_REPS as u32 {
                            STATE_REP_AFTER_LIT
                        } else {
                            STATE_MATCH_AFTER_LIT
                        }
                    } else {
                        STATE_REP_AFTER_LIT
                    };
                } else {
                    let pstate = self.opt[prev].state as usize;
                    state = if dist < NUM_REPS as u32 {
                        u32::from(REP_NEXT_STATES[pstate])
                    } else {
                        u32::from(MATCH_NEXT_STATES[pstate])
                    };
                }
                let prev_opt = self.opt[prev];
                let mut b0 = prev_opt.reps[0];
                if dist < NUM_REPS as u32 {
                    if dist == 0 {
                        reps[0] = b0;
                        reps[1] = prev_opt.reps[1];
                        reps[2] = prev_opt.reps[2];
                        reps[3] = prev_opt.reps[3];
                    } else {
                        reps[1] = b0;
                        b0 = prev_opt.reps[1];
                        if dist == 1 {
                            reps[0] = b0;
                            reps[2] = prev_opt.reps[2];
                            reps[3] = prev_opt.reps[3];
                        } else {
                            reps[2] = b0;
                            reps[0] = prev_opt.reps[dist as usize];
                            reps[3] = prev_opt.reps[(dist ^ 1) as usize];
                        }
                    }
                } else {
                    reps[0] = dist - NUM_REPS as u32 + 1;
                    reps[1] = b0;
                    reps[2] = prev_opt.reps[1];
                    reps[3] = prev_opt.reps[2];
                }
            }
            self.opt[cur].state = state;
            self.opt[cur].reps = reps;

            let data = position as usize;
            let cur_byte = self.input[data] as u32;
            let match_byte = self.input[data - reps[0] as usize] as u32;
            let pos_state = (position & self.pb_mask) as usize;
            let s = state as usize;

            let cur_price = self.opt[cur].price;
            let match_price = cur_price + self.prices.pp.price_1(self.probs.is_match[s][pos_state]);
            let mut lit_price = cur_price + self.prices.pp.price_0(self.probs.is_match[s][pos_state]);

            let mut next_is_lit = false;
            let next_price = self.opt[cur + 1].price;
            if (next_price < INFINITY_PRICE && match_byte == cur_byte) || lit_price > next_price {
                lit_price = 0;
            } else {
                let prev_b = self.input[data - 1] as u32;
                let base = self.lit_base(position, prev_b);
                let add = if !is_lit_state(state) {
                    lit_matched_get_price(&self.probs.literal[base..base + 0x300], cur_byte, match_byte, &self.prices.pp)
                } else {
                    lit_get_price(&self.probs.literal[base..base + 0x300], cur_byte, &self.prices.pp)
                };
                lit_price += add;
                if lit_price < next_price {
                    let o = &mut self.opt[cur + 1];
                    o.price = lit_price;
                    o.len = 1;
                    o.dist = MARK_LIT;
                    o.extra = 0;
                    next_is_lit = true;
                }
            }

            let rep_match_price = match_price + self.prices.pp.price_1(self.probs.is_rep[s]);

            let mut num_avail_full = self.num_avail;
            let temp = (NUM_OPTS - 1 - cur) as u32;
            if num_avail_full > temp {
                num_avail_full = temp;
            }

            // ---- SHORT_REP ----
            if is_lit_state(state) && match_byte == cur_byte && rep_match_price < self.opt[cur + 1].price {
                let nlen = self.opt[cur + 1].len;
                let ndist = self.opt[cur + 1].dist;
                if nlen < 2 || ndist != 0 {
                    let short_rep_price =
                        rep_match_price + price_short_rep(&self.probs, &self.prices.pp, s, pos_state);
                    if short_rep_price < self.opt[cur + 1].price {
                        let o = &mut self.opt[cur + 1];
                        o.price = short_rep_price;
                        o.len = 1;
                        o.dist = 0;
                        o.extra = 0;
                        next_is_lit = false;
                    }
                }
            }

            if num_avail_full < 2 {
                continue;
            }
            let num_avail = num_avail_full.min(self.num_fast_bytes);

            // ---- LIT : REP_0 ----
            if !next_is_lit && lit_price != 0 && match_byte != cur_byte && num_avail_full > 2 {
                let data2 = data - reps[0] as usize;
                if self.input[data + 1] == self.input[data2 + 1] && self.input[data + 2] == self.input[data2 + 2] {
                    let mut limit = (self.num_fast_bytes + 1) as usize;
                    if limit > num_avail_full as usize {
                        limit = num_avail_full as usize;
                    }
                    let mut l = 3usize;
                    while l < limit && self.input[data + l] == self.input[data2 + l] {
                        l += 1;
                    }
                    let state2 = LITERAL_NEXT_STATES[state as usize] as usize;
                    let pos_state2 = ((position + 1) & self.pb_mask) as usize;
                    let price = lit_price + price_rep_0(&self.probs, &self.prices.pp, state2, pos_state2);
                    let offset = cur + l;
                    if (last as usize) < offset {
                        last = offset as u32;
                    }
                    let l2 = (l - 1) as u32;
                    let price2 = price + self.prices.rep_len_enc.price(pos_state2, l2);
                    if price2 < self.opt[offset].price {
                        let o = &mut self.opt[offset];
                        o.price = price2;
                        o.len = l2;
                        o.dist = 0;
                        o.extra = 1;
                    }
                }
            }

            let mut start_len = 2u32;

            // ---- REP ----
            for rep_index in 0..NUM_REPS {
                let r = reps[rep_index] as usize;
                if self.input[data] != self.input[data - r] || self.input[data + 1] != self.input[data + 1 - r] {
                    continue;
                }
                let mut len_v = 2usize;
                while (len_v as u32) < num_avail && self.input[data + len_v] == self.input[data + len_v - r] {
                    len_v += 1;
                }
                let offset = cur + len_v;
                if (last as usize) < offset {
                    last = offset as u32;
                }
                let price = rep_match_price + price_pure_rep(&self.probs, &self.prices.pp, rep_index, s, pos_state);
                let mut len2 = len_v;
                loop {
                    let price2 = price + self.prices.rep_len_enc.price(pos_state, len2 as u32);
                    if price2 < self.opt[cur + len2].price {
                        let o = &mut self.opt[cur + len2];
                        o.price = price2;
                        o.len = len2 as u32;
                        o.dist = rep_index as u32;
                        o.extra = 0;
                    }
                    len2 -= 1;
                    if len2 < 2 {
                        break;
                    }
                }
                if rep_index == 0 {
                    start_len = (len_v + 1) as u32;
                }

                // ---- REP : LIT : REP_0 ----
                let mut limit = len_v + 1 + self.num_fast_bytes as usize;
                if limit > num_avail_full as usize {
                    limit = num_avail_full as usize;
                }
                let mut len2b = len_v + 3;
                let data2 = data - r;
                if len2b <= limit
                    && self.input[data + len2b - 2] == self.input[data2 + len2b - 2]
                    && self.input[data + len2b - 1] == self.input[data2 + len2b - 1]
                {
                    let state2 = REP_NEXT_STATES[s] as usize;
                    let pos_state2 = ((position + len_v as u32) & self.pb_mask) as usize;
                    let base = self.lit_base(position + len_v as u32, self.input[data + len_v - 1] as u32);
                    let mut price_b = price
                        + self.prices.rep_len_enc.price(pos_state, len_v as u32)
                        + self.prices.pp.price_0(self.probs.is_match[state2][pos_state2])
                        + lit_matched_get_price(
                            &self.probs.literal[base..base + 0x300],
                            self.input[data + len_v] as u32,
                            self.input[data2 + len_v] as u32,
                            &self.prices.pp,
                        );
                    let state2 = STATE_LIT_AFTER_REP as usize;
                    let pos_state2 = ((pos_state2 as u32 + 1) & self.pb_mask) as usize;
                    price_b += price_rep_0(&self.probs, &self.prices.pp, state2, pos_state2);
                    while len2b < limit && self.input[data + len2b] == self.input[data2 + len2b] {
                        len2b += 1;
                    }
                    len2b -= len_v;
                    let offset = cur + len_v + len2b;
                    if (last as usize) < offset {
                        last = offset as u32;
                    }
                    let l = (len2b - 1) as u32;
                    let price2 = price_b + self.prices.rep_len_enc.price(pos_state2, l);
                    if price2 < self.opt[offset].price {
                        let o = &mut self.opt[offset];
                        o.price = price2;
                        o.len = l;
                        o.extra = (len_v + 1) as u32;
                        o.dist = rep_index as u32;
                    }
                }
            }

            // ---- MATCH ----
            if new_len > num_avail {
                new_len = num_avail;
                let mut np = 0usize;
                while new_len > self.matches[np] {
                    np += 2;
                }
                self.matches[np] = new_len;
                np += 2;
                num_pairs = np as u32;
            }

            if new_len >= start_len {
                let normal_match_price = match_price + self.prices.pp.price_0(self.probs.is_rep[s]);
                let offset = cur + new_len as usize;
                if (last as usize) < offset {
                    last = offset as u32;
                }
                let mut offs = 0usize;
                while start_len > self.matches[offs] {
                    offs += 2;
                }
                let mut dist = self.matches[offs + 1];
                let mut pos_slot = optimum::get_pos_slot(dist);
                let mut mlen = start_len;
                loop {
                    let mut price = normal_match_price + self.prices.len_enc.price(pos_state, mlen);
                    let ltp = len_to_pos_state(mlen);
                    if dist < NUM_FULL_DISTANCES {
                        price += self.prices.distances_prices[ltp][(dist & (NUM_FULL_DISTANCES - 1)) as usize];
                    } else {
                        price += self.prices.pos_slot_prices[ltp][pos_slot as usize]
                            + self.prices.align_prices[(dist & ALIGN_MASK) as usize];
                    }
                    if price < self.opt[cur + mlen as usize].price {
                        let o = &mut self.opt[cur + mlen as usize];
                        o.price = price;
                        o.len = mlen;
                        o.dist = dist + NUM_REPS as u32;
                        o.extra = 0;
                    }
                    if mlen == self.matches[offs] {
                        // ---- MATCH : LIT : REP_0 ----
                        let data2 = data - dist as usize - 1;
                        let mut limit = mlen as usize + 1 + self.num_fast_bytes as usize;
                        if limit > num_avail_full as usize {
                            limit = num_avail_full as usize;
                        }
                        let mut len2 = mlen as usize + 3;
                        if len2 <= limit
                            && self.input[data + len2 - 2] == self.input[data2 + len2 - 2]
                            && self.input[data + len2 - 1] == self.input[data2 + len2 - 1]
                        {
                            while len2 < limit && self.input[data + len2] == self.input[data2 + len2] {
                                len2 += 1;
                            }
                            len2 -= mlen as usize;
                            let state2 = MATCH_NEXT_STATES[s] as usize;
                            let pos_state2 = ((position + mlen) & self.pb_mask) as usize;
                            let base = self.lit_base(position + mlen, self.input[data + mlen as usize - 1] as u32);
                            let mut price_m = price + self.prices.pp.price_0(self.probs.is_match[state2][pos_state2]);
                            price_m += lit_matched_get_price(
                                &self.probs.literal[base..base + 0x300],
                                self.input[data + mlen as usize] as u32,
                                self.input[data2 + mlen as usize] as u32,
                                &self.prices.pp,
                            );
                            let state2 = STATE_LIT_AFTER_MATCH as usize;
                            let pos_state2 = ((pos_state2 as u32 + 1) & self.pb_mask) as usize;
                            price_m += price_rep_0(&self.probs, &self.prices.pp, state2, pos_state2);
                            let offset2 = cur + mlen as usize + len2;
                            if (last as usize) < offset2 {
                                last = offset2 as u32;
                            }
                            let l = (len2 - 1) as u32;
                            let price2 = price_m + self.prices.rep_len_enc.price(pos_state2, l);
                            if price2 < self.opt[offset2].price {
                                let o = &mut self.opt[offset2];
                                o.price = price2;
                                o.len = l;
                                o.extra = mlen + 1;
                                o.dist = dist + NUM_REPS as u32;
                            }
                        }
                        offs += 2;
                        if offs as u32 == num_pairs {
                            break;
                        }
                        dist = self.matches[offs + 1];
                        pos_slot = optimum::get_pos_slot(dist);
                    }
                    mlen += 1;
                }
            }
        }

        // reset opt prices for next call (opt[last..=1] -> infinity)
        let mut l = last as usize;
        while l != 0 {
            self.opt[l].price = INFINITY_PRICE;
            l -= 1;
        }

        self.backward(cur)
    }

    fn run(mut self) -> Vec<u8> {
        let num_pos_states = 1usize << self.pb;

        // LzmaEnc_InitPrices
        self.prices.fill_distances_prices(&self.probs.pos_slot, &self.probs.spec_pos);
        self.prices.fill_align_prices(&self.probs.align);
        self.prices.rep_len_enc_counter = REP_LEN_COUNT;
        len_price_update(
            &mut self.prices.len_enc,
            num_pos_states,
            self.probs.len.choice,
            self.probs.len.choice2,
            &self.probs.len.low,
            &self.probs.len.mid,
            &self.probs.len.high,
            &self.prices.pp,
        );
        len_price_update(
            &mut self.prices.rep_len_enc,
            num_pos_states,
            self.probs.rep_len.choice,
            self.probs.rep_len.choice2,
            &self.probs.rep_len.low,
            &self.probs.rep_len.mid,
            &self.probs.rep_len.high,
            &self.prices.pp,
        );

        if self.mf.num_available() == 0 {
            // Empty input: nowPos is 0, which the C also passes here.
            return self.finish(0);
        }

        // First byte: always a literal (LzmaEnc.c:2405).
        self.read_match_distances();
        self.rc.encode_bit(&mut self.probs.is_match[0][0], 0);
        let first = self.input[0] as u32;
        {
            let table = &mut self.probs.literal[0..0x300];
            self.rc.encode_tree(table, 8, first);
        }
        self.additional_offset -= 1;

        let mut now_pos: u32 = 1;
        if self.mf.num_available() != 0 {
            loop {
                let len = if self.opt_end == self.opt_cur {
                    self.get_optimum(now_pos)
                } else {
                    let o = self.opt[self.opt_cur];
                    self.back_res = o.dist;
                    self.opt_cur += 1;
                    o.len
                };

                let dist = self.back_res;
                if dist == MARK_LIT {
                    self.encode_literal(now_pos as usize);
                } else if dist < NUM_REPS as u32 {
                    if dist == 0 && len == 1 {
                        self.encode_short_rep(now_pos as usize);
                    } else {
                        self.encode_rep(now_pos as usize, dist as usize, len);
                        self.prices.rep_len_enc_counter -= 1;
                    }
                } else {
                    self.encode_match(now_pos as usize, dist - NUM_REPS as u32 + 1, len);
                    self.prices.match_price_count += 1;
                }

                now_pos += len;
                self.additional_offset -= len;

                if self.additional_offset == 0 {
                    if self.prices.match_price_count >= 64 {
                        self.prices.fill_align_prices(&self.probs.align);
                        self.prices.fill_distances_prices(&self.probs.pos_slot, &self.probs.spec_pos);
                        len_price_update(
                            &mut self.prices.len_enc,
                            num_pos_states,
                            self.probs.len.choice,
                            self.probs.len.choice2,
                            &self.probs.len.low,
                            &self.probs.len.mid,
                            &self.probs.len.high,
                            &self.prices.pp,
                        );
                    }
                    if self.prices.rep_len_enc_counter <= 0 {
                        self.prices.rep_len_enc_counter = REP_LEN_COUNT;
                        len_price_update(
                            &mut self.prices.rep_len_enc,
                            num_pos_states,
                            self.probs.rep_len.choice,
                            self.probs.rep_len.choice2,
                            &self.probs.rep_len.low,
                            &self.probs.rep_len.mid,
                            &self.probs.rep_len.high,
                            &self.prices.pp,
                        );
                    }
                    if self.mf.num_available() == 0 {
                        break;
                    }
                }
            }
        }

        self.finish(now_pos)
    }
}
