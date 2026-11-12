"""Fake Claude TUI output: redraw a 12-line block 20 times a second (~26 KB/s)."""
import itertools, sys, time
spin = "◐◓◑◒"
out = sys.stdout
for n in itertools.count():
    frame = ["\x1b[?25l"] + (["\x1b[12A"] if n else [])
    for i in range(12):
        frame.append(f"\x1b[2K\x1b[38;5;{(n + i) % 200 + 20}m{spin[n % 4]} line {i:2d} frame {n:7d} "
                     + "lorem ipsum dolor sit amet consectetur " * 2 + "\x1b[0m\r\n")
    frame.append("\x1b[?25h")
    out.write("".join(frame)); out.flush()
    time.sleep(0.05)
