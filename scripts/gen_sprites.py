"""Generate minesweeper sprite PNGs into ../assets/.

Run: python3 scripts/gen_sprites.py
"""

from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

SIZE = 32
FLAG_BTN_H = 14  # height of the flag-toggle button below each cell
ASSETS = Path(__file__).resolve().parent.parent / "assets"
ASSETS.mkdir(exist_ok=True)

COVERED = (160, 160, 160)
REVEALED = (220, 220, 220)
BLACK = (0, 0, 0)
RED = (220, 30, 30)

NUM_COLORS = {
    1: (0, 0, 255),
    2: (0, 128, 0),
    3: (255, 0, 0),
    4: (0, 0, 128),
    5: (128, 0, 0),
    6: (0, 128, 128),
    7: (0, 0, 0),
    8: (128, 128, 128),
}


def load_font(size: int):
    candidates = [
        "/usr/share/fonts/TTF/DejaVuSans-Bold.ttf",
        "/usr/share/fonts/dejavu/DejaVuSans-Bold.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
        "/usr/share/fonts/noto/NotoSans-Bold.ttf",
    ]
    for p in candidates:
        try:
            return ImageFont.truetype(p, size)
        except OSError:
            continue
    return ImageFont.load_default()


def covered_base() -> Image.Image:
    return Image.new("RGB", (SIZE, SIZE), COVERED)


def revealed_base(bg=REVEALED) -> Image.Image:
    return Image.new("RGB", (SIZE, SIZE), bg)


def write_number(img: Image.Image, n: int):
    d = ImageDraw.Draw(img)
    font = load_font(SIZE * 22 // 32)
    text = str(n)
    bbox = d.textbbox((0, 0), text, font=font)
    w, h = bbox[2] - bbox[0], bbox[3] - bbox[1]
    x = (SIZE - w) // 2 - bbox[0]
    y = (SIZE - h) // 2 - bbox[1]
    d.text((x, y), text, fill=NUM_COLORS[n], font=font)


def draw_mine(img: Image.Image):
    d = ImageDraw.Draw(img)
    cx, cy = SIZE // 2, SIZE // 2
    r = SIZE * 8 // 32
    spike_len = SIZE * 4 // 32
    spike_w = max(2, SIZE // 16)
    refl = SIZE * 3 // 32
    d.ellipse([cx - r, cy - r, cx + r, cy + r], fill=BLACK)
    for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        d.line(
            [(cx + dx * (r - 1), cy + dy * (r - 1)),
             (cx + dx * (r + spike_len), cy + dy * (r + spike_len))],
            fill=BLACK, width=spike_w,
        )
    d.rectangle([cx - refl, cy - refl, cx - 1, cy - 1], fill=(255, 255, 255))


def draw_flag(img: Image.Image):
    d = ImageDraw.Draw(img)
    pole_x = SIZE * 12 // 32
    pole_top = SIZE * 6 // 32
    pole_bot = SIZE - pole_top
    pole_w = max(2, SIZE // 16)
    base_lh = SIZE * 8 // 32
    base_rh = SIZE * 5 // 32
    base_left = pole_x - SIZE * 4 // 32
    base_right = pole_x + SIZE * 6 // 32
    flag_w = SIZE * 12 // 32
    flag_mid = pole_top + SIZE * 5 // 32
    flag_bot = pole_top + SIZE * 10 // 32
    d.line([(pole_x, pole_top), (pole_x, pole_bot)], fill=BLACK, width=pole_w)
    d.rectangle([base_left, SIZE - base_lh, base_right, SIZE - base_rh], fill=BLACK)
    d.polygon(
        [(pole_x, pole_top), (pole_x + flag_w, flag_mid), (pole_x, flag_bot)],
        fill=RED, outline=BLACK,
    )


def flag_button() -> Image.Image:
    h = FLAG_BTN_H
    img = Image.new("RGB", (SIZE, h), COVERED)
    d = ImageDraw.Draw(img)
    pole_x = SIZE * 13 // 32
    pole_top = h * 3 // 14
    pole_bot = h - pole_top
    pole_w = max(1, SIZE // 32)
    flag_w = SIZE * 7 // 32
    flag_mid = pole_top + (pole_bot - pole_top) // 2
    d.line([(pole_x, pole_top), (pole_x, pole_bot)], fill=BLACK, width=pole_w)
    d.polygon(
        [(pole_x, pole_top), (pole_x + flag_w, flag_mid), (pole_x, pole_bot)],
        fill=RED, outline=BLACK,
    )
    return img


def save(img: Image.Image, name: str):
    out = ASSETS / f"{name}.png"
    img.save(out, "PNG", optimize=True)
    print(f"  wrote {out.relative_to(ASSETS.parent)}")


def main():
    save(covered_base(), "covered")
    save(revealed_base(), "0")
    for n in range(1, 9):
        img = revealed_base()
        write_number(img, n)
        save(img, str(n))
    img = revealed_base()
    draw_mine(img)
    save(img, "mine")
    img = revealed_base(RED)
    draw_mine(img)
    save(img, "mine_exploded")
    img = covered_base()
    draw_flag(img)
    save(img, "flag")
    save(flag_button(), "flag_button")


if __name__ == "__main__":
    main()
