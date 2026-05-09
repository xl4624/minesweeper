"""Print the HTML <table> of clickable minesweeper cells for the profile README.

Usage:
    python3 scripts/gen_readme.py [--server https://your.server.url] [--rows 9] [--cols 9]
"""

import argparse


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--server", default="https://YOUR-SERVER-HERE")
    p.add_argument("--rows", type=int, default=6)
    p.add_argument("--cols", type=int, default=6)
    p.add_argument("--size", type=int, default=32)
    p.add_argument("--flag-height", type=int, default=14)
    args = p.parse_args()

    base = args.server.rstrip("/")
    flag_btn = f"{base}/flag_button.png"

    print("<!-- BEGIN minesweeper -->")
    print('<table cellspacing="0" cellpadding="0" border="0">')
    for r in range(args.rows):
        print("  <tr>")
        for c in range(args.cols):
            click = f"{base}/click/{r}/{c}"
            flag = f"{base}/flag/{r}/{c}"
            cell = f"{base}/cell/{r}/{c}"
            print(
                f'    <td>'
                f'<a href="{click}"><img src="{cell}" '
                f'width="{args.size}" height="{args.size}" alt=""/></a>'
                f'<a href="{flag}"><img src="{flag_btn}" '
                f'width="{args.size}" height="{args.flag_height}" alt="flag"/></a>'
                f'</td>'
            )
        print("  </tr>")
    print("</table>")
    print("<!-- END minesweeper -->")


if __name__ == "__main__":
    main()
