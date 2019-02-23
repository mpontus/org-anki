# Org-Anki

> Create anki decks from the notes stored in org files.

## Usage

Write your notes in the following format:

```
* Deck Name
** Card 1 front
Card 1 back
** Card 2 front
Card 2 back
```

Navigate to the top-level heading and excute `M-x org-anki-sync` which will create a new deck and populate it with notes extracted from the subtree.


## Unsupported

- Renaming decks - no info on deck renaming in anki-connect documentation, saveDeckConfig does not work
- Card Deletion - no documentation in anki-connect

## How to extract heading body

Not as simple as (org-get-entry).

Only export from AST has an ability to separate property block from body

