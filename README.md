# Covid-Mobility
Analysis tools for Google Mobility data, DfT Transport data, tracking propensity for return to work, transport, etc. post-pandemic.

Non-chart R files contain two functions each: the first (`fetch_*`) will download the data onto your machine and the second (`process_*`) will process it.

The chart R files will look for objects in your environment with specific names: if it can't find them it will run the second function (`process_*`) and create it for you.

Feel free to drop problems or requests into the issues tab.
