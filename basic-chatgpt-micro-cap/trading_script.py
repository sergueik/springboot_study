"""Utilities for maintaining the ChatGPT micro cap portfolio.

The script processes portfolio positions, logs trades, and prints daily
results. It is intentionally lightweight and avoids changing existing
logic or behaviour.
"""

from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import yfinance as yf
from typing import Any, cast
import os
import time

# Shared file locations
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR  # Save files in the same folder as this script
PORTFOLIO_CSV = DATA_DIR / "chatgpt_portfolio_update.csv"
TRADE_LOG_CSV = DATA_DIR / "chatgpt_trade_log.csv"


def set_data_dir(data_dir: Path) -> None:
    """Update global paths for portfolio and trade logs.

    Parameters
    ----------
    data_dir:
        Directory where ``chatgpt_portfolio_update.csv`` and
        ``chatgpt_trade_log.csv`` are stored.
    """

    global DATA_DIR, PORTFOLIO_CSV, TRADE_LOG_CSV
    DATA_DIR = Path(data_dir)
    os.makedirs(DATA_DIR, exist_ok=True)
    PORTFOLIO_CSV = DATA_DIR / "chatgpt_portfolio_update.csv"
    TRADE_LOG_CSV = DATA_DIR / "chatgpt_trade_log.csv"

# Today's date reused across logs
today = datetime.today().strftime("%Y-%m-%d")
now = datetime.now()
day = now.weekday()



def process_portfolio(
    portfolio: pd.DataFrame | dict[str, list[object]] | list[dict[str, object]],
    cash: float,
) -> tuple[pd.DataFrame, float]:
    """Update daily price information, log stop-loss sells, and prompt for trades.

    Parameters
    ----------
    portfolio:
        Current holdings provided as a DataFrame, mapping of column names to
        lists, or a list of row dictionaries. The input is normalised to a
        ``DataFrame`` before any processing so that downstream code only deals
        with a single type.
    cash:
        Cash balance available for trading.

    Returns
    -------
    tuple[pd.DataFrame, float]
        Updated portfolio and cash balance.
    """
    print(portfolio)
    if isinstance(portfolio, pd.DataFrame):
        portfolio_df = portfolio.copy()
    elif isinstance(portfolio, (dict, list)):
        portfolio_df = pd.DataFrame(portfolio)
    else:  # pragma: no cover - defensive type check
        raise TypeError("portfolio must be a DataFrame, dict, or list of dicts")

    results: list[dict[str, object]] = []
    total_value = 0.0
    total_pnl = 0.0

    if day == 6 or day == 5:
        check = input("""Today is currently a weekend, so markets were never open. 
This will cause the program to calculate data from the last day (usually Friday), and save it as today.
Are you sure you want to do this? To exit, enter 1. """)
        if check == "1":
            raise SystemError("Exitting program...")

    while True:
        action = input(
            f""" You have {cash} in cash.
Would you like to log a manual trade? Enter 'b' for buy, 's' for sell, or press Enter to continue: """
        ).strip().lower()
        if action == "b":
            try:
                ticker = input("Enter ticker symbol: ").strip().upper()
                shares = float(input("Enter number of shares: "))
                buy_price = float(input("Enter buy price: "))
                stop_loss = float(input("Enter stop loss: "))
                if shares <= 0 or buy_price <= 0 or stop_loss <= 0:
                    raise ValueError
            except ValueError:
                print("Invalid input. Manual buy cancelled.")
            else:
                cash, portfolio_df = log_manual_buy(
                    buy_price, shares, ticker, stop_loss, cash, portfolio_df
                )
            continue
        if action == "s":
            try:
                ticker = input("Enter ticker symbol: ").strip().upper()
                shares = float(input("Enter number of shares to sell: "))
                sell_price = float(input("Enter sell price: "))
                if shares <= 0 or sell_price <= 0:
                    raise ValueError
            except ValueError:
                print("Invalid input. Manual sell cancelled.")
            else:
                cash, portfolio_df = log_manual_sell(
                    sell_price, shares, ticker, cash, portfolio_df
                )
            continue
        break

    for _, stock in portfolio_df.iterrows():
        ticker = stock["ticker"]
        shares = int(stock["shares"])
        cost = stock["buy_price"]
        stop = stock["stop_loss"]
        data = yf.Ticker(ticker).history(period="1d")

        if data.empty:
            print(f"No data for {ticker}")
            row = {
                "Date": today,
                "Ticker": ticker,
                "Shares": shares,
                "Cost Basis": cost,
                "Stop Loss": stop,
                "Current Price": "",
                "Total Value": "",
                "PnL": "",
                "Action": "NO DATA",
                "Cash Balance": "",
                "Total Equity": "",
            }
        else:
            low_price = round(float(data["Low"].iloc[-1]), 2)
            close_price = round(float(data["Close"].iloc[-1]), 2)

            if low_price <= stop:
                price = stop
                value = round(price * shares, 2)
                pnl = round((price - cost) * shares, 2)
                action = "SELL - Stop Loss Triggered"
                cash += value
                portfolio_df = log_sell(ticker, shares, price, cost, pnl, portfolio_df)
            else:
                price = close_price
                value = round(price * shares, 2)
                pnl = round((price - cost) * shares, 2)
                action = "HOLD"
                total_value += value
                total_pnl += pnl

            row = {
                "Date": today,
                "Ticker": ticker,
                "Shares": shares,
                "Cost Basis": cost,
                "Stop Loss": stop,
                "Current Price": price,
                "Total Value": value,
                "PnL": pnl,
                "Action": action,
                "Cash Balance": "",
                "Total Equity": "",
            }

        results.append(row)

    # Append TOTAL summary row
    total_row = {
        "Date": today,
        "Ticker": "TOTAL",
        "Shares": "",
        "Cost Basis": "",
        "Stop Loss": "",
        "Current Price": "",
        "Total Value": round(total_value, 2),
        "PnL": round(total_pnl, 2),
        "Action": "",
        "Cash Balance": round(cash, 2),
        "Total Equity": round(total_value + cash, 2),
    }
    results.append(total_row)

    df = pd.DataFrame(results)
    if PORTFOLIO_CSV.exists():
        existing = pd.read_csv(PORTFOLIO_CSV)
        existing = existing[existing["Date"] != today]
        print("rows for today already logged, not saving results to CSV...")
        time.sleep(1)
        df = pd.concat([existing, df], ignore_index=True)

    df.to_csv(PORTFOLIO_CSV, index=False)
    return portfolio_df, cash


def log_sell(
    ticker: str,
    shares: float,
    price: float,
    cost: float,
    pnl: float,
    portfolio: pd.DataFrame,
) -> pd.DataFrame:
    """Record a stop-loss sale in ``TRADE_LOG_CSV`` and remove the ticker."""
    log = {
        "Date": today,
        "Ticker": ticker,
        "Shares Sold": shares,
        "Sell Price": price,
        "Cost Basis": cost,
        "PnL": pnl,
        "Reason": "AUTOMATED SELL - STOPLOSS TRIGGERED",
    }

    portfolio = portfolio[portfolio["ticker"] != ticker]

    if TRADE_LOG_CSV.exists():
        df = pd.read_csv(TRADE_LOG_CSV)
        df = pd.concat([df, pd.DataFrame([log])], ignore_index=True)
    else:
        df = pd.DataFrame([log])
    df.to_csv(TRADE_LOG_CSV, index=False)
    return portfolio


def log_manual_buy(
    buy_price: float,
    shares: float,
    ticker: str,
    stoploss: float,
    cash: float,
    chatgpt_portfolio: pd.DataFrame,
) -> tuple[float, pd.DataFrame]:
    """Log a manual purchase and append to the portfolio."""
    check = input(
        f"""You are currently trying to buy {shares} shares of {ticker} with a price of {buy_price} and a stoploss of {stoploss}.
        If this a mistake, type "1". """
    )
    if check == "1":
        print("Returning...")
        return cash, chatgpt_portfolio

    data = yf.download(ticker, period="1d")
    data = cast(pd.DataFrame, data)
    if data.empty:
        print(f"Manual buy for {ticker} failed: no market data available.")
        return cash, chatgpt_portfolio
    day_high = float(data["High"].iloc[-1].item())
    day_low = float(data["Low"].iloc[-1].item())
    if not (day_low <= buy_price <= day_high):
        print(
            f"Manual buy for {ticker} at {buy_price} failed: price outside today's range {round(day_low, 2)}-{round(day_high, 2)}."
        )
        return cash, chatgpt_portfolio
    if buy_price * shares > cash:
        print(
            f"Manual buy for {ticker} failed: cost {buy_price * shares} exceeds cash balance {cash}."
        )
        return cash, chatgpt_portfolio
    pnl = 0.0

    log = {
        "Date": today,
        "Ticker": ticker,
        "Shares Bought": shares,
        "Buy Price": buy_price,
        "Cost Basis": buy_price * shares,
        "PnL": pnl,
        "Reason": "MANUAL BUY - New position",
    }

    if os.path.exists(TRADE_LOG_CSV):
        df = pd.read_csv(TRADE_LOG_CSV)
        df = pd.concat([df, pd.DataFrame([log])], ignore_index=True)
    else:
        df = pd.DataFrame([log])
    df.to_csv(TRADE_LOG_CSV, index=False)
    # if the portfolio doesn't already contain ticker, create a new row.
    
    mask = chatgpt_portfolio["ticker"] == ticker

    if not mask.any():
        new_trade = {
            "ticker": ticker,
            "shares": shares,
            "stop_loss": stoploss,
            "buy_price": buy_price,
            "cost_basis": buy_price * shares,
        }
        chatgpt_portfolio = pd.concat(
            [chatgpt_portfolio, pd.DataFrame([new_trade])], ignore_index=True
        )
    else:
        row_index = chatgpt_portfolio[mask].index[0]
        current_shares = float(chatgpt_portfolio.at[row_index, "shares"])
        chatgpt_portfolio.at[row_index, "shares"] = current_shares + shares
        current_cost_basis = float(chatgpt_portfolio.at[row_index, "cost_basis"])
        chatgpt_portfolio.at[row_index, "cost_basis"] = shares * buy_price + current_cost_basis
        chatgpt_portfolio.at[row_index, "stop_loss"] = stoploss
    cash = cash - shares * buy_price
    print(f"Manual buy for {ticker} complete!")
    return cash, chatgpt_portfolio


def log_manual_sell(
    sell_price: float,
    shares_sold: float,
    ticker: str,
    cash: float,
    chatgpt_portfolio: pd.DataFrame,
) -> tuple[float, pd.DataFrame]:
    """Log a manual sale and update the portfolio."""
    reason = input(
        f"""You are currently trying to sell {shares_sold} shares of {ticker} at a price of {sell_price}.
If this is a mistake, enter 1. """
    )

    if reason == "1":
        print("Returning...")
        return cash, chatgpt_portfolio
    if ticker not in chatgpt_portfolio["ticker"].values:
        print(f"Manual sell for {ticker} failed: ticker not in portfolio.")
        return cash, chatgpt_portfolio
    ticker_row = chatgpt_portfolio[chatgpt_portfolio["ticker"] == ticker]

    total_shares = int(ticker_row["shares"].item())
    if shares_sold > total_shares:
        print(
            f"Manual sell for {ticker} failed: trying to sell {shares_sold} shares but only own {total_shares}."
        )
        return cash, chatgpt_portfolio
    data = yf.download(ticker, period="1d")
    data = cast(pd.DataFrame, data)
    if data.empty:
        print(f"Manual sell for {ticker} failed: no market data available.")
        return cash, chatgpt_portfolio
    day_high = float(data["High"].iloc[-1])
    day_low = float(data["Low"].iloc[-1])
    if not (day_low <= sell_price <= day_high):
        print(
            f"Manual sell for {ticker} at {sell_price} failed: price outside today's range {round(day_low, 2)}-{round(day_high, 2)}."
        )
        return cash, chatgpt_portfolio
    buy_price = float(ticker_row["buy_price"].item())
    cost_basis = buy_price * shares_sold
    pnl = sell_price * shares_sold - cost_basis
    log = {
        "Date": today,
        "Ticker": ticker,
        "Shares Bought": "",
        "Buy Price": "",
        "Cost Basis": cost_basis,
        "PnL": pnl,
        "Reason": f"MANUAL SELL - {reason}",
        "Shares Sold": shares_sold,
        "Sell Price": sell_price,
    }
    if os.path.exists(TRADE_LOG_CSV):
        df = pd.read_csv(TRADE_LOG_CSV)
        df = pd.concat([df, pd.DataFrame([log])], ignore_index=True)
    else:
        df = pd.DataFrame([log])
    df.to_csv(TRADE_LOG_CSV, index=False)

    if total_shares == shares_sold:
        chatgpt_portfolio = chatgpt_portfolio[chatgpt_portfolio["ticker"] != ticker]
    else:
        row_index = ticker_row.index[0]
        chatgpt_portfolio.at[row_index, "shares"] = total_shares - shares_sold
        chatgpt_portfolio.at[row_index, "cost_basis"] = (
            chatgpt_portfolio.at[row_index, "shares"]
            * chatgpt_portfolio.at[row_index, "buy_price"]
        )

    cash = cash + shares_sold * sell_price
    print(f"manual sell for {ticker} complete!")
    return cash, chatgpt_portfolio


def daily_results(chatgpt_portfolio: pd.DataFrame, cash: float) -> None:
    """Print daily price updates and performance metrics."""
    portfolio_dict: list[dict[str, object]] = chatgpt_portfolio.to_dict(orient="records")

    print(f"prices and updates for {today}")
    time.sleep(1)
    for stock in portfolio_dict + [{"ticker": "^RUT"}] + [{"ticker": "IWO"}] + [{"ticker": "XBI"}]:
        ticker = stock["ticker"]
        try:
            data = yf.download(ticker, period="2d", progress=False)
            data = cast(pd.DataFrame, data)
            if data.empty or len(data) < 2:
                print(f"Data for {ticker} was empty or incomplete.")
                continue
            price = float(data["Close"].iloc[-1].item())
            last_price = float(data["Close"].iloc[-2].item())

            percent_change = ((price - last_price) / last_price) * 100
            volume = float(data["Volume"].iloc[-1].item())
        except Exception as e:
            raise Exception(f"Download for {ticker} failed. {e} Try checking internet connection.")
        print(f"{ticker} closing price: {price:.2f}")
        print(f"{ticker} volume for today: ${volume:,}")
        print(f"percent change from the day before: {percent_change:.2f}%")
    chatgpt_df = pd.read_csv(PORTFOLIO_CSV)

    # Filter TOTAL rows and get latest equity
    chatgpt_totals = chatgpt_df[chatgpt_df["Ticker"] == "TOTAL"].copy()
    chatgpt_totals["Date"] = pd.to_datetime(chatgpt_totals["Date"])
    final_date = chatgpt_totals["Date"].max()
    final_value = chatgpt_totals[chatgpt_totals["Date"] == final_date]
    final_equity = float(final_value["Total Equity"].values[0])
    equity_series = chatgpt_totals["Total Equity"].astype(float).reset_index(drop=True)

    # Daily returns
    daily_pct = equity_series.pct_change().dropna()

    total_return = (equity_series.iloc[-1] - equity_series.iloc[0]) / equity_series.iloc[0] 

    # Number of total trading days
    n_days = len(chatgpt_totals)
    # Risk-free return over total trading period (assuming 4.5% risk-free rate)
    rf_annual = 0.045
    rf_period = (1 + rf_annual) ** (n_days / 252) - 1
    # Standard deviation of daily returns
    std_daily = daily_pct.std()
    negative_pct = daily_pct[daily_pct < 0]
    negative_std = negative_pct.std()
    # Sharpe Ratio
    sharpe_total = (total_return - rf_period) / (std_daily * np.sqrt(n_days))
    # Sortino Ratio
    sortino_total = (total_return - rf_period) / (negative_std * np.sqrt(n_days))

    # Output
    print(f"Total Sharpe Ratio over {n_days} days: {sharpe_total:.4f}")
    print(f"Total Sortino Ratio over {n_days} days: {sortino_total:.4f}")
    print(f"Latest ChatGPT Equity: ${final_equity:.2f}")
    # Get S&P 500 data
    spx = yf.download("^SPX", start="2025-06-27", end=final_date + pd.Timedelta(days=1), progress=False)
    spx = cast(pd.DataFrame, spx)
    spx = spx.reset_index()

    # Normalize to $100
    initial_price = spx["Close"].iloc[0].item()
    price_now = spx["Close"].iloc[-1].item()
    scaling_factor = 100 / initial_price
    spx_value = price_now * scaling_factor
    print(f"$100 Invested in the S&P 500: ${spx_value:.2f}")
    print("today's portfolio:")
    print(chatgpt_portfolio)
    print(f"cash balance: {cash}")

    print(
        "Here are is your update for today. You can make any changes you see fit (if necessary),\n"
        "but you may not use deep research. You do have to ask premissons for any changes, as you have full control.\n"
        "You can however use the Internet and check current prices for potenial buys."
    )


def main(file: str, data_dir: Path | None = None) -> None:
    """Run the trading script.

    Parameters
    ----------
    file:
        CSV file containing historical portfolio records.
    data_dir:
        Directory where trade and portfolio CSVs will be stored.
    """
    chatgpt_portfolio, cash = load_latest_portfolio_state(file)
    if data_dir is not None:
        set_data_dir(data_dir)

    chatgpt_portfolio, cash = process_portfolio(chatgpt_portfolio, cash)
    daily_results(chatgpt_portfolio, cash)

def load_latest_portfolio_state(
    file: str,
) -> tuple[pd.DataFrame | list[dict[str, Any]], float]:
    """Load the most recent portfolio snapshot and cash balance.

    Parameters
    ----------
    file:
        CSV file containing historical portfolio records.

    Returns
    -------
    tuple[pd.DataFrame | list[dict[str, Any]], float]
        A representation of the latest holdings (either an empty DataFrame or a
        list of row dictionaries) and the associated cash balance.
    """

    df = pd.read_csv(file)
    if df.empty:
        portfolio = pd.DataFrame([])
        print(
            "Portfolio CSV is empty. Returning set amount of cash for creating portfolio."
        )
        try:
            cash = float(input("What would you like your starting cash amount to be? "))
        except ValueError:
            raise ValueError(
                "Cash could not be converted to float datatype. Please enter a valid number."
            )
        return portfolio, cash
    non_total = df[df["Ticker"] != "TOTAL"].copy()
    non_total["Date"] = pd.to_datetime(non_total["Date"])

    latest_date = non_total["Date"].max()

    # Get all tickers from the latest date
    latest_tickers = non_total[non_total["Date"] == latest_date].copy()
    latest_tickers.drop(columns=["Date", "Cash Balance", "Total Equity", "Action", "Current Price", "PnL", "Total Value"], inplace=True)
    latest_tickers.rename(columns={"Cost Basis": "buy_price", "Shares": "shares", "Ticker": "ticker", "Stop Loss": "stop_loss"}, inplace=True)
    latest_tickers['cost_basis'] = latest_tickers['shares'] * latest_tickers['buy_price']
    latest_tickers = latest_tickers.reset_index(drop=True).to_dict(orient='records')
    df = df[df["Ticker"] == "TOTAL"]  # Only the total summary rows
    df["Date"] = pd.to_datetime(df["Date"])
    latest = df.sort_values("Date").iloc[-1]
    cash = float(latest["Cash Balance"])
    return latest_tickers, cash

