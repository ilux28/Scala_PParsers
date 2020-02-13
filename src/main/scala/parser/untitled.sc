val str = """"20"*''"""

str.contains("(\"|\')" )
str.replaceAll("(\"|\')", """*""")

if (str.contains(""""""")) {
  str.replaceAll("\"", """\'""")
} else if (str.contains("""*""")) {
  str.replaceAll("""\*""", """.*""")
}