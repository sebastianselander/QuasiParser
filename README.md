# Btw
A lot of the code is taken from [glguy's](https://github.com/glguy/advent/) version of the same thing.
This was mostly an opportunity to practice parsing and meta programming in Haskell


# Example

## CSV
### Values are digits

```hs
[fmt|%d&(,)|]
```
This will parse `1,2,3,4,5` fine.
To allow leading and trailing space we can do the following

```hs
[fmt|%d&(( )*,( )*)|]
```

Now we can parse `1,2,   3,2      ,     4` just fine.

Can do much much more things too. Can't be arsed writing it all out atm.

E.g. to parse day 20 of advent of code

```hs
parser :: String -> [(String, [String])]
parser = [fmt|(((%y?%s)! -> %s&(, ))%n)*|]
```
