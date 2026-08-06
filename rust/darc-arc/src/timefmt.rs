//! Local-time formatting for listings — shared by `darc` and `unarc`.
//!
//! Lifted out of `bin/darc.rs` so `unarc l`/`v` can render the same column as
//! `darc l`/`v`. A second copy would be a second answer to "what time is this
//! archive claiming", which is the class of duplication the `Unarc/` port
//! exists to end.
//!
//! mtimes are formatted in LOCAL time, as `System.Time`'s `toCalendarTime`
//! does. Reproduced rather than corrected: matching the reference is the bar.

/// mtimes are formatted in LOCAL time, as `System.Time`'s `toCalendarTime`
/// does. Reproduced rather than corrected: matching the reference is the bar.
pub fn format_time(t: i64) -> String {
    let secs = t + local_offset_seconds();
    let days = secs.div_euclid(86_400);
    let tod = secs.rem_euclid(86_400);
    let (y, m, d) = civil_from_days(days);
    format!("{y:04}-{m:02}-{d:02} {:02}:{:02}:{:02}", tod / 3600, (tod % 3600) / 60, tod % 60)
}

pub fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if m <= 2 { y + 1 } else { y }, m, d)
}

#[cfg(not(windows))]
pub fn local_offset_seconds() -> i64 {
    // SAFETY: localtime_r writes into a tm we own; time 0 is always valid.
    unsafe {
        let t: i64 = 0;
        let mut tm: Tm = std::mem::zeroed();
        localtime_r(&t, &mut tm);
        tm.tm_gmtoff
    }
}

#[cfg(not(windows))]
#[repr(C)]
struct Tm {
    tm_sec: i32,
    tm_min: i32,
    tm_hour: i32,
    tm_mday: i32,
    tm_mon: i32,
    tm_year: i32,
    tm_wday: i32,
    tm_yday: i32,
    tm_isdst: i32,
    tm_gmtoff: i64,
    tm_zone: *const i8,
}

#[cfg(not(windows))]
extern "C" {
    fn localtime_r(t: *const i64, tm: *mut Tm) -> *mut Tm;
}

/// The Windows CRT has neither `localtime_r` nor `tm_gmtoff` — its `struct tm`
/// stops at `tm_isdst`. The offset comes out of a round trip instead: break
/// epoch 0 down in LOCAL time, then reassemble those same fields as if they
/// were UTC. The difference from 0 is exactly the offset, and `_mkgmtime64`
/// returns it directly.
#[cfg(windows)]
pub fn local_offset_seconds() -> i64 {
    #[repr(C)]
    struct TmW {
        tm_sec: i32,
        tm_min: i32,
        tm_hour: i32,
        tm_mday: i32,
        tm_mon: i32,
        tm_year: i32,
        tm_wday: i32,
        tm_yday: i32,
        tm_isdst: i32,
    }
    // The explicitly-64-bit names, not `localtime`/`mkgmtime`: those are macros
    // whose time_t width depends on how the CRT headers were configured, and
    // this passes an i64. `_localtime64` rather than `_localtime64_s` because
    // the secure variant is not in every msvcrt.dll, while the plain one is in
    // both msvcrt (x86_64-pc-windows-gnu) and UCRT (the gnullvm targets).
    extern "C" {
        fn _localtime64(t: *const i64) -> *mut TmW;
        fn _mkgmtime64(tm: *mut TmW) -> i64;
    }
    // SAFETY: time 0 is always valid; the returned pointer is CRT-owned static
    // storage, read and passed straight back before anything else can call into
    // the CRT's time functions.
    unsafe {
        let t: i64 = 0;
        let tm = _localtime64(&t);
        // Null means no usable zone. UTC is the honest fallback, and it is what
        // the Unix path yields for a zone with no offset.
        match tm.is_null() {
            true => 0,
            false => _mkgmtime64(tm),
        }
    }
}

