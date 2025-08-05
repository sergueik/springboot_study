## Q&A (Written July 26)

### Why did you remove DeepSeek after Week 1? Wouldn’t it be valuable to compare models?
I didn’t want to fully remove it at first. I considered feeding it stock reports and letting it trade off that. But that would’ve broken the core rule: the model has to find opportunities independently. If I start feeding it filtered ideas, it's no longer testing true AI-driven alpha.

### Why were you using R2K and XBI (BioTech EFT) as comparsions?
I intially only used Russell 2000, but I added XBI later after ChatGPT heavily leaned towards BioTech. Of course, looking back on it, it was a terrible idea to add metrics later. From now on, I will purely use the S&P 500 as that is the standard baseline for tests.
### Do you ever switch chats?
Yes. I wish I didn’t have to, but after ~2 weeks the performance slows down too much to be usable. I always keep chats organized in the same project and reintroduce prior theses when switching, though some changes are inevitable. It’s not perfect, but it keeps continuity.

### If you could start over, what would you do differently?
I’d make sure prompts are 100% consistent from the beginning and confirm the simulation engine was airtight before logging trades.

Can I ask you a few questions? Is chatgpt automatically running the prompts you shared on github? or do you prompt it every day? are you or is the script placing the buy/sell orders?

### Is prompting automated?

Right now, no. All prompts are manual, but I do my best to keep things consisent.  I would love to add it using ChatGPT's API, but it would cause problems like executing trades in the in middle of the week. Absolutely doable, but I wanna focus on making it user friendly right now.

### What about buying and selling?

Also no. All live trades are manual currently. However for the simuation, it is slightly easier to place orders via input() when running Trading Script. Of course, still needs work.

### What strengths or weaknesses have you noticed in the model?
**Strengths**: It’s incredible at finding overlooked data in obscure filings, especially biotech reports nobody reads. It also seems to great at capitalizing on short-term momentum.
**Weaknesses**: It often struggles with patience. It’s unclear if it can hold through the full timeline of a catalyst. And switching chats can cause it to lose conviction and restructure prematurely.

### Any plans after the 6 months?
If I can keep up during school, I might extend this idea into a full-year version.

### What return are you expecting by the end?
Originally, I expected 5–10%. But with the kind of asymmetric moves it’s hit so far, I think 25–30% isn’t out of the question, if it can manage risk well.

### Why are you using 4o? Doesn't ChatGPT have better models?
To be honest, I didn't think about it and just used the defult model. I may switch over to a more advanced one, but I will explictly state when and what model. 

### How do you ensure the limit orders get filled?
I mirror the trades in a real Vanguard brokerage account. If it executes there, I log the fill in the simulation.

### Can I create my own portfolio with the given code?
If you know Python decently well, you can do it. However you will have to delete existing data from the CSV files and edit to portfolio dictionary to change tickers. Sadly, for non coders there's not a way to do it with out editting or deleting code. However, there may be a massive update soon that could change that! So stay tuned.

### Could this actually be a real investing strategy?
It has serious potential. Most retail traders chase SPY predictions with random forests and fail. Real alpha hides in footnotes, obscure filings, and early-stage clinical data, files an LLM can parse in seconds. If combined with human oversight, this could potentially be a viable hybrid strategy. Of course, much more rigorous testing is needed. 

Want to submit a question? 

Please do so here: nathanbsmith.business@gmail.com
