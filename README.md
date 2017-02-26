# hmemo

Spaced repetition of flash cards on the command line, using simple csv-like files.

## Example
```
> # tab-separated csv with vocabulary
> cat german.csv
correct richtig
false   falsch
something       etwas
> hmemo german.csv
Front:  correct
Back:  >richtig
Yes!    richtig
Rating:>5

Front:  false
Back:  >falsh
No.     falsch
Repeat:>falsch
Rating:>1

Front:  something
Back:  >^D

Now reviewing mistakes

Front:  false
Back:  >falsch
Yes!    falsch
Rating:>4

Done!
> cat german.csv
correct	richtig	2017-02-26T11:41:28	5
false	falsch	2017-02-26T11:41:32	1
something	etwas
```

## Install

```
git clone https://github.com/mbid/hmemo
cd hmemo
stack build && stack install
~/.local/bin/hmemo --help
```
