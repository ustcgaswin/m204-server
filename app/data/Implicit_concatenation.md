## Implicit concatenation

Implicit concatenation eliminates the need to precede User Language variables (including methods chained off a variable) and literals with the operator `With` to indicate concatenation. For example, the following statement sets `%foo` to the contents of `%test` enclosed within parentheses:

`%foo = "(" %test ")"`

And the following statement sets `%foo` to the reversed, right-most 4 characters of `%test` enclosed within parentheses:

`%foo = "(" %test:right(4):reverse ")"`

If `%nal` is a Named Arraylist of String, implicit concatenation lets you assign to `%foo` the value of item `%x + 1` embedded inside single quotes:

`%foo = '%nal(%x + 1)'`

The at sign (`@`) will also be implicitly concatenated if not preceded by an operator. For example, the following concatenates a `==>` to the current contents of `%string` and assigns it to `%string`:

`%string = '==>' @`

In Sirius Mods 8.1 and later, the results of a `$function` will also be implicitly concatenated with the preceding expression results:

`%message = "The problem happened at $time on $date"`

With is still required before field names:

`%foo = '>>' with FIELD NAME`

And `With` is still required before expressions in parentheses:

`%foo = '>>' with (x + 2)`
