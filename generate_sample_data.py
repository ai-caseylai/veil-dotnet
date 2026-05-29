#!/usr/bin/env python3
"""
Veil RFM Analytics — Sample Data Generator
Generates synthetic retail transaction data for testing.
"""

import argparse
import csv
import os
import random
import lzma
import sys
from datetime import datetime, timedelta

# ── Product catalog ─────────────────────────────────────────────────────────
PRODUCTS_7FANS = [
    ("P00001", "Coke 330ml",            "BEV", "DAI"),
    ("P00002", "Coke Zero 330ml",       "BEV", "DAI"),
    ("P00003", "Green Tea 500ml",       "BEV", "DAI"),
    ("P00004", "Orange Juice 1L",       "BEV", "DAI"),
    ("P00005", "Beer 500ml",            "ALC", "DAI"),
    ("P00006", "Milk 1L",               "DAI", "DAI"),
    ("P00007", "Bread White",           "GRO", "BAK"),
    ("P00008", "Sandwich Ham",          "GRO", "BAK"),
    ("P00009", "Rice Ball",             "GRO", "SNK"),
    ("P00010", "Instant Noodle",        "GRO", "SNK"),
    ("P00011", "Potato Chips",          "SNK", "SNK"),
    ("P00012", "Chocolate Bar",         "SNK", "SNK"),
    ("P00013", "Chewing Gum",           "SNK", "SNK"),
    ("P00014", "Tissue Box",            "HOU", "HOU"),
    ("P00015", "Shampoo 200ml",         "HBA", "HBA"),
    ("P00016", "Battery AA",            "HOU", "HOU"),
    ("P00017", "Magazine",              "MAG", "MAG"),
    ("P00018", "Ice Cream Cup",         "FFD", "FFD"),
    ("P00019", "Cigarette A",           "TBC", "TBC"),
]

PRODUCTS_BAUHAUS = [
    ("P00101", "Slim Fit Shirt",        "APP", "MEN"),
    ("P00102", "Denim Jeans",           "APP", "MEN"),
    ("P00103", "Cashmere Sweater",      "APP", "WOM"),
    ("P00104", "Silk Blouse",           "APP", "WOM"),
    ("P00105", "Leather Jacket",        "APP", "OUT"),
    ("P00106", "Wool Coat",             "APP", "OUT"),
    ("P00107", "Cotton T-Shirt",        "APP", "BAS"),
    ("P00108", "Linen Trousers",        "APP", "BAS"),
    ("P00109", "Leather Belt",          "ACC", "ACC"),
    ("P00110", "Silk Scarf",            "ACC", "ACC"),
    ("P00111", "Canvas Tote",           "ACC", "BAG"),
    ("P00112", "Leather Handbag",       "ACC", "BAG"),
    ("P00113", "Running Sneakers",      "SHO", "SHO"),
    ("P00114", "Leather Loafers",       "SHO", "SHO"),
    ("P00115", "Ankle Boots",           "SHO", "SHO"),
]

PRICE_RANGE = {
    "BEV": (6, 15), "ALC": (12, 25), "DAI": (8, 20),
    "GRO": (5, 18), "SNK": (5, 15), "HOU": (10, 40),
    "HBA": (25, 80), "MAG": (15, 45), "FFD": (8, 30), "TBC": (55, 70),
    "APP": (80, 400), "ACC": (30, 350), "SHO": (100, 800),
    "MEN": (80, 400), "WOM": (80, 400), "OUT": (150, 800),
    "BAS": (60, 250), "BAG": (100, 600),
}

# Each segment: (label, weight, recency_days, frequency, avg_spend)
MEMBER_SEGMENTS = [
    ("best",        0.10, (1, 7),    (20, 30), (40, 80)),
    ("loyal",       0.20, (3, 21),   (12, 20), (30, 60)),
    ("potential",   0.15, (1, 14),   (6, 12),  (25, 50)),
    ("new",         0.10, (1, 30),   (2, 6),   (15, 40)),
    ("at_risk",     0.15, (45, 120), (10, 20), (30, 55)),
    ("cannot_lose", 0.10, (60, 180), (15, 25), (50, 90)),
    ("lost",        0.10, (120, 365),(3, 10),  (10, 30)),
    ("hibernating", 0.10, (30, 90),  (3, 8),   (10, 30)),
]


def weighted_choice(choices, rng):
    total = sum(w for _, w, *_ in choices)
    r = rng.random() * total
    cum = 0
    for item in choices:
        cum += item[1]
        if r <= cum:
            return item
    return choices[-1]


def generate_order_dates(segment, start_date, end_date, num_orders, rng):
    _, _, recency_range, _freq_range, _ = segment
    dates = []
    total_days = (end_date - start_date).days
    for _ in range(num_orders):
        max_recency = min(rng.randint(*recency_range), total_days)
        day_offset = total_days - rng.randint(0, max_recency)
        base_date = start_date + timedelta(days=day_offset)
        hour = rng.randint(6, 23)
        minute = rng.randint(0, 59)
        second = rng.randint(0, 59)
        dates.append(base_date.replace(hour=hour, minute=minute, second=second))
    return sorted(dates)


def generate(args):
    rng = random.Random(args.seed)
    os.makedirs(args.output_dir, exist_ok=True)

    products = PRODUCTS_BAUHAUS if args.bu == 106 else PRODUCTS_7FANS
    start = datetime.strptime(args.start, "%Y-%m-%d")
    end = datetime.strptime(args.end, "%Y-%m-%d")

    # ── Members ──────────────────────────────────────────────────────────
    members = []
    for i in range(args.members):
        member_id = f"M{i+1:06d}"
        card_number = str(rng.randint(1000000000, 9999999999))
        segment = weighted_choice(MEMBER_SEGMENTS, rng)
        gender = rng.randint(0, 1)
        members.append({
            "MemberID": member_id, "CardNumber": card_number,
            "segment": segment, "Gender": gender,
        })

    # ── Assign orders per member ─────────────────────────────────────────
    member_orders = {}
    for m in members:
        _, _, _, fr, _ = m["segment"]
        member_orders[m["MemberID"]] = rng.randint(fr[0], fr[1])

    # ── Generate rows ────────────────────────────────────────────────────
    rows = []
    order_counter = 1
    for m in members:
        mid, card, gender, seg = m["MemberID"], m["CardNumber"], m["Gender"], m["segment"]
        order_dates = generate_order_dates(seg, start, end, member_orders[mid], rng)
        for od in order_dates:
            order_id = f"O{order_counter:08d}"
            order_counter += 1
            n_items = rng.choices([1, 2, 3, 4], weights=[0.4, 0.35, 0.15, 0.10])[0]
            for _ in range(n_items):
                pid, pname, cat, dept = rng.choice(products)
                lo, hi = PRICE_RANGE.get(cat, (5, 50))
                rows.append({
                    "CardNumber": card, "MemberID": mid, "OrderID": order_id,
                    "Timestamp": od.strftime("%Y-%m-%d %H:%M:%S"),
                    "ProductID": pid, "ProdName1": pname,
                    "Category": cat, "DepartCode": dept,
                    "NetPrice": round(rng.uniform(lo, hi), 2),
                    "Quantity": rng.choices([1, 2, 3], weights=[0.6, 0.3, 0.1])[0],
                    "Gender": gender,
                })

    rows.sort(key=lambda r: r["Timestamp"])

    # ── Write ────────────────────────────────────────────────────────────
    fieldnames = ["CardNumber","MemberID","OrderID","Timestamp",
                  "ProductID","ProdName1","Category","DepartCode",
                  "NetPrice","Quantity","Gender"]

    csv_path = os.path.join(args.output_dir, args.output.replace(".xz", ""))
    with open(csv_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    xz_path = os.path.join(args.output_dir, args.output)
    with open(csv_path, "rb") as f_in, lzma.open(xz_path, "wb") as f_out:
        f_out.write(f_in.read())
    os.remove(csv_path)

    # ── Stats ────────────────────────────────────────────────────────────
    unique_members = len(set(r["MemberID"] for r in rows))
    unique_orders = len(set(r["OrderID"] for r in rows))
    print(f"Generated: {xz_path}")
    print(f"  BU:           {args.bu} ({'Bauhaus' if args.bu == 106 else '7Fans'})")
    print(f"  Rows:         {len(rows)}")
    print(f"  Members:      {unique_members}")
    print(f"  Orders:       {unique_orders}")
    print(f"  Date range:   {rows[0]['Timestamp']} — {rows[-1]['Timestamp']}")
    seg_counts = {}
    for m in members:
        n = m["segment"][0]
        seg_counts[n] = seg_counts.get(n, 0) + 1
    print(f"  Segments:     {', '.join(f'{k}={v}' for k,v in sorted(seg_counts.items()))}")


def main():
    p = argparse.ArgumentParser(
        description="Veil RFM Analytics — Sample Data Generator",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s
  %(prog)s --members 200 --seed 123
  %(prog)s --bu 106 --start 2024-01-01 --end 2025-12-31
  %(prog)s --members 500 --output-dir RIUData/7Fans/txn/daily
        """.strip())
    p.add_argument("--bu", type=int, default=107, choices=[106, 107],
                   help="Business unit: 106=Bauhaus, 107=7Fans (default: 107)")
    p.add_argument("--members", type=int, default=50,
                   help="Number of unique members (default: 50)")
    p.add_argument("--start", default="2025-01-01",
                   help="Start date YYYY-MM-DD (default: 2025-01-01)")
    p.add_argument("--end", default="2026-05-29",
                   help="End date YYYY-MM-DD (default: 2026-05-29)")
    p.add_argument("--seed", type=int, default=42,
                   help="Random seed for reproducibility (default: 42)")
    p.add_argument("--output-dir", default="RIUData/7Fans/txn",
                   help="Output directory (default: RIUData/7Fans/txn)")
    p.add_argument("--output", default="sample_txn.csv.xz",
                   help="Output filename (default: sample_txn.csv.xz)")
    generate(p.parse_args())


if __name__ == "__main__":
    main()
