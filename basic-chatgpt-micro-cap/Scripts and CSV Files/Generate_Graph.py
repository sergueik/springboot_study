"""Plot ChatGPT portfolio performance against the S&P 500.

The script loads logged portfolio equity, fetches S&P 500 data, and
renders a comparison chart. Core behaviour remains unchanged; the code
is simply reorganised and commented for clarity.
"""

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
import yfinance as yf
from typing import cast

DATA_DIR = Path(__file__).resolve().parent
PORTFOLIO_CSV = DATA_DIR / "chatgpt_portfolio_update.csv"


def parse_date(date_str: str, label: str) -> pd.Timestamp:
    """Safely parse a ``YYYY-MM-DD`` string into a timestamp."""

    try:
        return pd.to_datetime(date_str)
    except ValueError as exc:  # pragma: no cover - user input validation
        msg = f"Invalid {label} '{date_str}'. Use the YYYY-MM-DD format."
        raise SystemExit(msg) from exc


def load_portfolio_details(
    baseline_equity: float, baseline_date: pd.Timestamp | None
) -> pd.DataFrame:
    """Load portfolio equity history and prepend a baseline row.

    Parameters
    ----------
    baseline_equity:
        Dollar value used for the synthetic starting equity.
    baseline_date:
        Date assigned to the baseline equity. If ``None`` the earliest
        portfolio date is used. When the CSV has no data the baseline date is
        set to ``pd.Timestamp.today()``.
    """

    if not PORTFOLIO_CSV.exists():
        msg = (
            f"Portfolio file '{PORTFOLIO_CSV}' not found. Run Trading_Script.py "
            "to generate it."
        )
        raise SystemExit(msg)

    chatgpt_df = pd.read_csv(PORTFOLIO_CSV)
    chatgpt_totals = chatgpt_df[chatgpt_df["Ticker"] == "TOTAL"].copy()
    chatgpt_totals["Date"] = pd.to_datetime(chatgpt_totals["Date"])

    if baseline_date is None:
        if not chatgpt_totals.empty:
            baseline_date = chatgpt_totals["Date"].min()
        else:
            baseline_date = pd.Timestamp.today()

    baseline_row = pd.DataFrame({"Date": [baseline_date], "Total Equity": [baseline_equity]})
    return pd.concat([baseline_row, chatgpt_totals], ignore_index=True).sort_values("Date")


def download_sp500(start_date: pd.Timestamp, end_date: pd.Timestamp) -> pd.DataFrame:
    """Download S&P 500 prices and normalise to a $100 baseline."""
    sp500 = yf.download(
        "^SPX", start=start_date, end=end_date + pd.Timedelta(days=1), progress=False
    )
    sp500 = cast(pd.DataFrame, sp500)
    sp500 = sp500.reset_index()
    if isinstance(sp500.columns, pd.MultiIndex):
        sp500.columns = sp500.columns.get_level_values(0)
    spx_27_price = 6173.07
    scaling_factor = 100 / spx_27_price
    sp500["SPX Value ($100 Invested)"] = sp500["Close"] * scaling_factor
    return sp500


def main(
    baseline_equity: float, start_date: pd.Timestamp | None, end_date: pd.Timestamp | None
) -> None:
    """Generate and display the comparison graph."""
    if baseline_equity <= 0:
        raise SystemError("Baseline equity must be positive.")

    chatgpt_totals = load_portfolio_details(baseline_equity, start_date)

    min_portfolio = chatgpt_totals["Date"].min()
    max_portfolio = chatgpt_totals["Date"].max()

    if start_date is None:
        start_date = min_portfolio
    if end_date is None:
        end_date = max_portfolio

    if start_date < min_portfolio:
        print(
            "Start date before portfolio history; using",
            min_portfolio.date(),
        )
        start_date = min_portfolio
    if end_date > max_portfolio:
        print(
            "End date after portfolio history; using",
            max_portfolio.date(),
        )
        end_date = max_portfolio
    if start_date > end_date:
        raise SystemExit("Start date must be on or before end date.")

    sp500 = download_sp500(start_date, end_date)

    plt.figure(figsize=(10, 6))
    plt.style.use("seaborn-v0_8-whitegrid")
    plt.plot(
        chatgpt_totals["Date"],
        chatgpt_totals["Total Equity"],
        label="ChatGPT ($100 Invested)",
        marker="o",
        color="blue",
        linewidth=2,
    )
    plt.plot(
        sp500["Date"],
        sp500["SPX Value ($100 Invested)"],
        label="S&P 500 ($100 Invested)",
        marker="o",
        color="orange",
        linestyle="--",
        linewidth=2,
    )

    final_date = chatgpt_totals["Date"].iloc[-1]
    final_chatgpt = float(chatgpt_totals["Total Equity"].iloc[-1])
    final_spx = sp500["SPX Value ($100 Invested)"].iloc[-1]

    plt.text(
        final_date, final_chatgpt + 0.3, f"+{final_chatgpt - baseline_equity:.1f}%", color="blue", fontsize=9
    )
    plt.text(
        final_date, final_spx + 0.9, f"+{final_spx - 100:.1f}%", color="orange", fontsize=9
    )
    plt.title("ChatGPT's Micro Cap Portfolio vs. S&P 500")
    plt.xlabel("Date")
    plt.ylabel("Value of $100 Investment")
    plt.xticks(rotation=15)
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Plot portfolio performance")
    parser.add_argument(
        "--baseline-equity",
        type=float,
        default=100.0,
        help="Starting equity value used for normalisation",
    )
    parser.add_argument(
        "--start-date",
        type=str,
        help="Start date for the chart (YYYY-MM-DD)",
    )
    parser.add_argument(
        "--end-date",
        type=str,
        help="End date for the chart (YYYY-MM-DD)",
    )
    args = parser.parse_args()

    start = parse_date(args.start_date, "start date") if args.start_date else None
    end = parse_date(args.end_date, "end date") if args.end_date else None

    main(args.baseline_equity, start, end)

