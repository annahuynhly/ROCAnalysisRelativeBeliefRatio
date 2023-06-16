
colour_theme_list = list("Default Theme 1" = 'default1',
                         "Default Theme 2" = 'default2',
                         "Dull Life" = 'dull',
                         "Lovely Mei" = 'lovelymei',
                         "Execute" = "jackin",
                         "Manually Insert" = 'manual')

# Difference here is that there is a customized option to use from the prev. section
colour_theme_list_custom = list("Default Theme 1" = 'default1',
                                "Default Theme 2" = 'default2',
                                "Dull Life" = 'dull',
                                "Lovely Mei" = 'lovelymei',
                                "Execute" = 'jackin',
                                "Custom Theme from AUC Plots" = 'custom',
                                "Manually Insert" = 'manual')

output_line_list = list("Prior" = 'prior',
                        "Posterior" = 'post',
                        "Relative Belief Ratio" = 'rbr',
                        "Line of y = 1" = 'line_1',
                        "Credible Region Line" = "cr")

basic_colour_list = list("Red" = 'red', 
                         "Blue" = 'blue', 
                         "Green" = 'green', 
                         "Manually Insert" = 'manual')

colour_theme_w0 = list("Relative Belief Ratio" = 'rbr',
                       "Relative Belief Ratio of w0" = 'rbr_w0',
                       "Interval" = 'interval',
                       "Strength (Area)" = "str")

default_lty_list = list("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6)

# Note: base R : pch values go from 1 to 25
default_copt_list = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                         "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
                         "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15,
                         "16" = 16, "17" = 17, "18" = 18, "19" = 19, "20" = 20,
                         "21" = 21, "22" = 22, "23" = 23, "24" = 24, "25" = 25)

# Used colour list (pr and cr REMOVED)
default1 = c("#FF6666", "#6699FF", "#05DEB2", "#3333FF", "#5b10a7")
default2 = c("blue", "green", "red", "royalblue1", "#81ddff")
dull = c("#EE4266", "#3cbbb1", "#b33c86", "#0a0f0d", "#3185fc")
lovelymei = c("#3800c2", "#676bf8", "#58887a", "#372f66", "#a2cda3")
jackin_execute = c("#0092d6", "#212c57", "#f85210", "#0092d6", "#da1a1a")

# The actual colour list (ORIGINAL, when we had the pr and cr)
default1_full = c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
default2_full = c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
dull_full = c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
lovelymei_full = c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
jackin_execute_full = c("#0092d6", "#212c57", "#f85210", "#ffc710", "#0092d6", "#da1a1a")

