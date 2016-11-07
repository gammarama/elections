sorted.data <- spend.data[with(spend.data, order(-exp_amo)),]

sorted.data[12,]
# Opposition ad by American Crossroads. 7.9 million.
# TV ad. http://www.usnews.com/news/blogs/Ken-Walshs-Washington/2012/07/20/super-pac-american-crossroads-becoming-key-to-romney-campaign

sorted.data[14,]
# Support ad by Restore Our Future.  6.8 Million.
# http://www.huffingtonpost.com/2012/07/30/restore-our-future_n_1719310.html

subset(sorted.data, bucket2 == "direct contact" & beneful_can == "romney" & sup_opp == "support" & exp_dat > "2012-09-14" & exp_dat < "2012-10-01")

