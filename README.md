# Escoger

Escoger is a terminal fuzzy selector. I lifted the idea and scoring
system from [selecta](https://github.com/garybernhardt/selecta).

Anytime you need to select one thing from a list of things you can use
escoger.

Some examples:

```bash
# Simple example
cat $(find * -type f | escoger)

# Some aliases that I like
alias eh='cd $(find ~/code/haskell -maxdepth 1 -type d | escoger)'
alias eb='git checkout $(git branch | cut -c3- | escoger)'
alias ecd='cd $(find * -name .git -a -type d -prune -o -type d -print | escoger)'
```