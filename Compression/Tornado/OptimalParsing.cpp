main
{
    // 1. generate the possible matches
    for (i=0; i<CHUNK; i++)
    {
        matches[i] = matchp;                  // save the pointer to the first match for this position
        matchp = fill_matches(buf,i,matchp);  // generate all the matches for this position and write them into the buffer
    }
    // 1.5 Supplement the matches list with pointers to 2/3-byte strings
    // 2. choose the best path backwards
    iterate (CHUNK, price[i]=INT_MAX);  price[0]=0;
    for (i=0; i<CHUNK; i++)
    {
        suggest (i+1, 1, buf[i], price[i] + charPrice(buf[i]));  // suggest for position i+1 the current match + a character
        lastlen = MINMATCH-1;
        for (our matches)
            while (++lastlen <= len)   // fill in all the vacancies from the length of the previous match up to the length of the current one (todo: if len>256, fill the first and last 128 elements and skip over everything in between)
            {
                // todo: if the distance coincides with one of the 4 previous ones, the price will be lower..
                suggest (i+lastlen, lastlen, dist, price[i] + matchPrice(lastlen,dist)); // price = the price of the current match + the encoding of the new one
            }
    }
    // 3. Write the optimal path from the end to the beginning
    for (i=CHUNK-1; i; i-=len[i])
    {
        push (len[i], dist[i]);
    }
    // 4. Encode the found path to Salvation
    while (stack not empty)
    {
        len, dist = pop();
        encode (len,dist);
    }
}

suggest (i, len, dist, match_price)
{
    if (price[i] < match_price)        // the new variant turned out to be cheaper
    {
        price[i] = match_price;
        len[i]  = len;
        dist[i] = dist;
    }
}
