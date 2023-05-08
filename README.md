# Forecast Analysis ggplots

Scripts to create ggplots for past performance analysis.

Requires extract from SCeye forecasting with the SKU/month level information:

-   Date

-   Categorical

    -   GBU, Franchise, Region, Country (⚠️ splits some markets like France and Dom-Tom), Enrichment rule (0-touch/Enrichment), Asset (Global/Local Core, Foundation), MAPE Exclude Rupture flag

-   Sales and Forecast (⚠️ Markets where Stat Forecast scope is in a channel show some discrepancies)

    -   Stat Forecast and Final Forecast

    -   Sales with stat forecast and Total Sales
