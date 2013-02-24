regex-crossword
===============

Solver for the regex crossword puzzle that was featured on Hacker News. It solves it within a second on my machine. It works by expanding the regexes into a list of length one regexes. It then intersects these with each other, taking care to propagate information from groups and backreferences.
