# Start Your Own

This folder lets you run the trading experiment on your own computer. It contains two small scripts and the CSV files they produce.

Run the commands below from the repository root. The scripts automatically
save their CSV data inside this folder.

## Trading_Script.py

This script updates your portfolio and logs trades.

1. **Install Python packages**
   ```bash
   pip install pandas yfinance numpy matplotlib
   ```
2. **Run the script**
   ```bash
   python "Start Your Own/Trading_Script.py"
   ```
3. **Follow the prompts**
   - The program uses past data from 'chatgpt_portfolio_update.csv' to automatically grab today's portfolio.
   - If it is a weekend, the script will inform you that date will be inaccurate. However, this is easily fixable by editing CSV files manually and saving.
   - If 'chatgpt_portfolio_update.csv' is empty (meaning no past trading days logged), you will required to enter your starting cash.
   - From here, you can set up your portfolio or make any changes.
   - The script asks if you want to record manual buys or sells. **IT WILL ASSUME TRADES HAPPEN, SO CHECK ACCURACY.**
   - After you hit 'Enter' all calculations for the day are made.
   - Results are saved to `chatgpt_portfolio_update.csv` and any trades are added to `chatgpt_trade_log.csv`.
   - In the terminal, daily results are printed. Copy and paste results into the LLM. **ALL PROMPTING IS MANUAL AT THE MOMENT.**

## Generate_Graph.py

This script draws a graph of your portfolio versus the S&P 500.

1. **Ensure you have portfolio data**
   - Run `Trading_Script.py` at least once so `chatgpt_portfolio_update.csv` has data.
2. **Run the graph script**
   ```bash
   python "Start Your Own/Generate_Graph.py" --baseline-equity 100
   ```
   - Optional flags `--start-date` and `--end-date` accept dates in `YYYY-MM-DD` format. For example:
   ```bash
   python "Start Your Own/Generate_Graph.py" --baseline-equity 100 --start-date 2023-01-01 --end-date 2023-12-31
   ```
3. **View the chart**
   - A window opens showing your portfolio value vs. S&P 500. Results will be adjusted for baseline equity.

All of this is still VERY NEW, so there is bugs. Please reach out if you find an issue or have a question.

Both scripts are designed for beginners, feel free to experiment and modify them as you learn.
