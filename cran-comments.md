## Response to CRAN comments

> The Description field is intended to be a (one paragraph) description of
> what the package does and why it may be useful. Please add more details
> about the package functionality and implemented methods in your
> Description text.
> For more details:
> <https://contributor.r-project.org/cran-cookbook/general_issues.html#description-length>

Thank you, I have expanded the Description field. It now states:

Description: Tools for transforming 'R' expressions. Provides functions for 
 finding, extracting, and replacing patterns in 'R' language objects, similarly 
 to how regular expressions can be used to find, extract, and replace patterns 
 in text. Also provides functions for generating code using specially-formatted 
 template files and for translating 'R' expressions into similar expressions in 
 other programming languages. The package may be helpful for advanced uses of 
 'R' expressions, such as developing domain-specific languages.

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'https:' and angle brackets for
> auto-linking. (If you want to add a title as well please put it in
> quotes: "Title")
> For more details:
> <https://contributor.r-project.org/cran-cookbook/description_issues.html#references>

Thanks, there are no suitable references at present.

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)
> For more details:
> <https://contributor.r-project.org/cran-cookbook/docs_issues.html#missing-value-tags-in-.rd-files>
> 
> -> Missing Rd-tags:
>      subset-.expr_alt.Rd: \value

Thank you, this change has been made.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
