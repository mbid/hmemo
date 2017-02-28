# hmemo

Spaced repetition of flash cards on the command line, using simple csv-like files.

## Usage

Create a tsv file (csv, but with tabs instead of commas) containing your flash cards.
The first column should be the front of the cards, i.e. the question, and the second column should be the the answer.
```
> cat german.tsv
correct richtig
false   falsch
something       etwas
...
```

Now run `hmemo german.tsv`.
You can also split up your flash cards in multiple desks and list all of them in order,i.e.  `hmemo german1.tsv german2.tsv ...`.

Some statistics about the decks are printed, and then a learning session starts.
The [SuperMemo2 algorithm](https://www.supermemo.com/english/ol/sm2.htm) is used to decide when to review a flash card.
This means that after each question, you're asked to rate how good your answer was, on a scale from 0 to 5.

| Score | Description |
| :---: | --- |
| 5 | perfect response |
| 4 | correct response after a hesitation |
| 3 | correct response recalled with serious difficulty |
| 2 | incorrect response but the correct one seemed easy to recall |
| 1 | incorrect response but the correct one remembered |
| 0 | complete blackout |

This table is not enforced, so you can rate a card higher than 2 although you didn't give the correct response.
Be honest to yourself and only do this if you only mistyped something but actually knew the right spelling.

Upon entering Ctrl-d instead of the answer, hmemo will wrap up the session by repeating all cards that scored below 4 until they are rated at least 4.
SuperMemo2 suggests learning once a day.



The meta data of the learning session is written back to the input files.
If your file looked like this:
```
correct richtig
false   falsch
something       etwas
```
and you quit after the first answer, rated 4, the file will look like this:
```
correct richtig 2017-02-28T09:40:59	4
false   falsch
something       etwas
```
The third column contains a timestamp of the last review and the fourth is a the list of all past ratings.
Lines with cards that were not reviewed yet are left untouched.
This means that the resulting tsv file is actually malformed because not all its rows have the same number of fields.

To add a new card, simply add a new line containing two fields.
If you want to remove the meta data about your learning progress, i.e. the third and fourth column, use
```bash
awk -F "\t" '{print $1 "\t" $2}' german.tsv
```

## Install

Currently only via [stack](https://docs.haskellstack.org/en/stable/README/), which you can probably install via your distribution's package manager.

```bash
git clone https://github.com/mbid/hmemo
cd hmemo
stack build && stack install
~/.local/bin/hmemo --help
```
