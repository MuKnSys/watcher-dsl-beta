# ERC20 and Uniswap V2 Watchers

[specs](https://docs.google.com/document/d/1gDyuC78L_BdqCin9_N_iAUvyHVA-v0WEXEdTiFYO6q4)

This folder contains a collection of watchers that demonstrate various use cases for monitoring and analyzing ERC20 tokens and Uniswap V2 pairs using our custom Domain Specific Language (DSL). These examples are ordered from the simplest to the most complex.

Please note that there might be slight differences from the specifications of the DSL, but they have been accounted for and should not affect the key design ideas.

## Watchers
- ERC20 Total Supply Watcher ([ERC20TotalSupplyWatcher.watcher](ERC20TotalSupplyWatcher.watcher)) - This is the simplest watcher, which queries the total supply of an ERC20 token.

- ERC20 Holders Watcher ([ERC20HoldersWatcher.watcher](ERC20HoldersWatcher.watcher)) - This watcher fetches the list of holders of an ERC20 token with a balance greater than a specified amount.

- ERC20 Scammer Ratio Watcher ([ERC20ScammerRatioWatcher.watcher](ERC20ScammerRatioWatcher.watcher)) - This watcher monitors the transactions of an ERC20 token and calculates the ratio of scammers to non-scammers who are selling the tokens.

- Uniswap V2 Pair Price Watcher ([UniswapV2PairPriceWatcher.watcher](UniswapV2PairPriceWatcher.watcher)) - This watcher fetches the reserves and calculates the price for a Uniswap V2 pair.

- Uniswap V2 Pair Analytics Watcher ([UniswapV2PairAnalyticsWatcher.watcher](UniswapV2PairAnalyticsWatcher.watcher) - This is the most complex watcher, which performs various financial analysis functions on the price of a Uniswap V2 pair, such as calculating the current price, percentage change, moving average, and standard deviation.

*For a detailed explanation of each watcher and the code it contains, please refer to the individual watcher files and their respective comments.
If something does not make sense, do not hestiate and reach to me (Marcin) so we can disucss. 
Semantics of this is not trivial, end expects lot of postporcessing on compiler side and can vary depending on deployment context.*




