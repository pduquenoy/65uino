#!/usr/bin/env python3
"""
Generate combined LUT entries for Si5351 for use on the 6502 project.

Each record produced is 17 bytes:
  - 8 bytes: PLL registers 26..33 (encoded P3,P3,P1[17:16]|..., P1[15:8], P1[7:0], P2[19:16], P2[15:8], P2[7:0])
  - 9 bytes: MS table expected by `write_ms0_ms1` (8 bytes used as pattern + 1 byte phase offset / integer divider)

The MS integer divider (last byte) is clamped to 127 to match hardware constraints.

Usage:
  ./tools/gen_si5351_lut.py 7000000 7020000 7040000

The script prints assembly-friendly `.byte` lines named `frq_<hz>_data:`.
"""
from fractions import Fraction
import sys

# Configuration constants
XTAL = 25_000_000
VCO_MIN = 600_000_000
VCO_MAX = 900_000_000
MS_MIN = 4
MS_MAX = 900
# Constrain MS integer used for the phase offset byte to <=127
MS_MAX_FOR_TABLE = 127
MAX_DEN = (1 << 20) - 1


def best_ms_for(f_out, max_allowed=MS_MAX_FOR_TABLE):
    """Choose an integer MS (<= max_allowed) so VCO = f_out * MS is within VCO range.
    Prefer the smallest MS that fits. If none fits, pick the value <= max_allowed
    that brings VCO closest to the VCO mid point.
    """
    top = min(MS_MAX, max_allowed)
    candidates = [ms for ms in range(MS_MIN, top + 1) if VCO_MIN <= f_out * ms <= VCO_MAX]
    if candidates:
        return min(candidates)
    mid = (VCO_MIN + VCO_MAX) / 2
    return min(range(MS_MIN, top + 1), key=lambda m: abs(f_out * m - mid))


def pll_fraction(vco):
    N = Fraction(vco, XTAL)
    frac = N.limit_denominator(MAX_DEN)
    a = frac.numerator // frac.denominator
    b = frac.numerator - a * frac.denominator
    c = frac.denominator
    return a, b, c


def p_values(a, b, c):
    x = (128 * b) // c
    P1 = 128 * a + x - 512
    P2 = 128 * b - c * x
    P3 = c
    return P1, P2, P3


def pack_pll_regs(P1, P2, P3):
    r26 = (P3 >> 8) & 0xFF
    r27 = P3 & 0xFF
    r28 = ((P1 >> 16) & 0x03) | (((P3 >> 16) & 0x0F) << 4)
    r29 = (P1 >> 8) & 0xFF
    r30 = P1 & 0xFF
    r31 = ((P2 >> 16) & 0x0F)
    r32 = (P2 >> 8) & 0xFF
    r33 = P2 & 0xFF
    return [r26, r27, r28, r29, r30, r31, r32, r33]


def pack_ms_regs(P1, P2, P3, ms_int):
    # Format expected by write_ms0_ms1: 8 bytes (pattern) + 1 byte phase offset/int divider
    r42 = (P3 >> 8) & 0xFF
    r43 = P3 & 0xFF
    r44 = ((P1 >> 16) & 0x03)
    r45 = (P1 >> 8) & 0xFF
    r46 = P1 & 0xFF
    r47 = ((P2 >> 16) & 0x0F)
    r48 = (P2 >> 8) & 0xFF
    r49 = P2 & 0xFF
    return [r42, r43, r44, r45, r46, r47, r48, r49, ms_int & 0xFF]


def fmt_bytes(bs):
    return ", ".join(str(x) for x in bs)


def make_entry(f_out):
    ms = best_ms_for(f_out)
    vco = f_out * ms
    a, b, c = pll_fraction(vco)
    P1, P2, P3 = p_values(a, b, c)
    pll_bytes = pack_pll_regs(P1, P2, P3)

    msP1 = 128 * ms - 512
    ms_bytes = pack_ms_regs(msP1, 0, 1, ms)
    if ms_bytes[-1] > MS_MAX_FOR_TABLE:
        ms_bytes[-1] = MS_MAX_FOR_TABLE

    return {
        'f_out': f_out,
        'ms': ms,
        'vco': vco,
        'pll': (a, b, c),
        'P': (P1, P2, P3),
        'pll_regs': pll_bytes,
        'ms_bytes': ms_bytes
    }


if __name__ == "__main__":
    # Require explicit mode flags to avoid accidental huge outputs
    import argparse

    parser = argparse.ArgumentParser(description="Generate SI5351 LUT entries (PLL+MS table)\nEither use --seq start step count, --list f1 f2.. or --file filename with one freq per line.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--seq', nargs=3, metavar=('START','STEP','COUNT'), help='Generate a sequence: start_hz step_hz count')
    group.add_argument('--list', nargs='*', metavar='FREQ', help='Provide individual frequencies on the command line')
    group.add_argument('--file', metavar='FILE', help='Read one frequency per line from FILE')
    parser.add_argument('--max', type=int, default=2000, help='Maximum number of entries to generate (safety)')
    args = parser.parse_args()

    freqs = []
    if args.seq:
        try:
            start = int(args.seq[0])
            step = int(args.seq[1])
            count = int(args.seq[2])
        except ValueError:
            parser.error('start/step/count must be integers')
        if count <= 0 or count > args.max:
            parser.error(f'count must be 1..{args.max}')
        freqs = [start + i * step for i in range(count)]
        table_name_hint = f"{start}_{step}"
    elif args.list is not None:
        if len(args.list) == 0:
            freqs = []
        else:
            try:
                freqs = [int(x) for x in args.list]
            except ValueError as e:
                parser.error('All --list frequencies must be integers')
        if len(freqs) > args.max:
            parser.error(f'you requested {len(freqs)} entries, which exceeds --max {args.max}')
        table_name_hint = 'list'
    else:
        # --file
        try:
            with open(args.file, 'r') as fh:
                for ln in fh:
                    s = ln.split('#',1)[0].strip()
                    if not s:
                        continue
                    freqs.append(int(s))
        except Exception as e:
            parser.error(f'Error reading file: {e}')
        if len(freqs) > args.max:
            parser.error(f'file contains {len(freqs)} entries, which exceeds --max {args.max}')
        table_name_hint = args.file.replace('.', '_')

    labels = []
    first = True
    for f in freqs:
        e = make_entry(f)
        ms = e['ms']
        vco = e['vco']
        a, b, c = e['pll']
        P1, P2, P3 = e['P']
        pll_bytes = e['pll_regs']
        ms_bytes = e['ms_bytes']
        # Print a compact one-line metadata comment per entry
        print(f"; freq={f}Hz MS={ms} VCO={vco}Hz PLL={a},{b},{c} P={P1},{P2},{P3}")

        # Emit verbose register-definition comments only once before the first entry
        if first:
            reg_comments = (
                "; PLL regs 26..33: MSNA P3[15:8], P3[7:0], P1[17:16]+..., P1[15:8], P1[7:0], P2[19:16], P2[15:8], P2[7:0]",
                "; MS0 regs 42..49: P3,P3,P1[..],P1[15:8],P1[7:0],P2[..],P2[15:8],P2[7:0]",
                "; MS1 regs 50..57: P3,P3,P1[..],P1[15:8],P1[7:0],P2[..],P2[15:8],P2[7:0]",
                "; Phase/Integer divider: reg 165 (phase offset for CLK0) - last byte"
            )
            for rc in reg_comments:
                print(rc)
            first = False
        label = f"frq_{f}_data"
        labels.append(label)
        print(f"{label}: .byte {fmt_bytes(pll_bytes)}, {fmt_bytes(ms_bytes)}")
        print()

    # Emit a pointer table (.word entries) for the generated frequencies
    if len(freqs) > 0:
        if len(freqs) == 1:
            table_name = "freq_table_single"
        else:
            table_name = f"freq_table_{table_name_hint}"

        print(f"; Pointer table for generated frequencies")
        print(f"{table_name}:")
        for lab in labels:
            print(f"  .word {lab}")

    sys.exit(0)
