## wildrwolf 0.5 

I was asked to fix the following issues: 

- I have updated the Authors@R field. 
- I have deleted any statements via "cat()"
- I have added information on return values
- I have added links to methods papers in the description 
- I have not switched \dontrun to \donttest, as the example is a 'big' simulation that runs for a very long time (rather one hour than one minute). 

I have checked the package on 
- github actions (windows, mac, ubuntu)
- rhub
- win-devel

All tests produces no errors and warnings, but the following two notes: the usual detrius comment + complaint about misspelled fixest (false positive).


Attached are the comments I received: 

Please rather use the Authors@R field and declare Maintainer, Authors
and Contributors with their appropriate roles with person() calls.
e.g. something like:
Authors@R: c(person("Alice", "Developer", role = c("aut", "cre","cph"),
email = "alice.developer@some.domain.net"),
person("Bob", "Dev", role = "aut") )

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      run_fwer_sim.Rd: \value
      summary.rwolf.Rd: \value

\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \dontrun with \donttest.

Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.

You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning() or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to
the console. (except for print, summary, interactive functions)
-> R/rwolf_sim.R

Please fix and resubmit.

## wildrwolf 0.4

- checked on local windows installation 
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- checked on github actions (mac, windows, ubuntu)
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- checked on rhub: 
  * Note:    checking for detritus in the temp directory ... Found the following files/directories:'lastMiKTeXException'

- checked on win-devel
  * Note: Possibly misspelled words in DESCRIPTION: fixest (9:31, 9:44) - it's spelled correctly
