# indexwc

## Introduction
A system to run and summarize models that estimate indices of
abundance for various data sources specific to populations off the
U.S. West Coast. Models can range in complexity from simple linear
models to complex spatio-temporal models with lots of random effects
and non-linear relationships.

## Running
Indices for a large set of species/survey combinations can be run by sourcing the `data-raw/configuration.R` file. The package is currently not set up to run a single index or an index for a single species, but you can modify `data-raw/configurations.R` to filter a subset of the rows in `data-raw/configuration.csv` using a command like
`configuration <- configuration |> dplyr::filter(species == "yellowtail rockfish")`.

## Contributors


<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the [`allcontributors` package](https://github.com/ropensci/allcontributors) following the [all-contributors](https://allcontributors.org) specification. Contributions of any kind are welcome!

### Code

<table>

<tr>
<td align="center">
<a href="https://github.com/kellijohnson-NOAA">
<img src="https://avatars.githubusercontent.com/u/4108564?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/commits?author=kellijohnson-NOAA">kellijohnson-NOAA</a>
</td>
<td align="center">
<a href="https://github.com/seananderson">
<img src="https://avatars.githubusercontent.com/u/19349?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/commits?author=seananderson">seananderson</a>
</td>
<td align="center">
<a href="https://github.com/chantelwetzel-noaa">
<img src="https://avatars.githubusercontent.com/u/6172110?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/commits?author=chantelwetzel-noaa">chantelwetzel-noaa</a>
</td>
<td align="center">
<a href="https://github.com/iantaylor-NOAA">
<img src="https://avatars.githubusercontent.com/u/4992918?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/commits?author=iantaylor-NOAA">iantaylor-NOAA</a>
</td>
<td align="center">
<a href="https://github.com/ericward-noaa">
<img src="https://avatars.githubusercontent.com/u/5046884?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/commits?author=ericward-noaa">ericward-noaa</a>
</td>
</tr>

</table>


### Issue Authors

<table>

<tr>
<td align="center">
<a href="https://github.com/okenk">
<img src="https://avatars.githubusercontent.com/u/205013?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/issues?q=is%3Aissue+author%3Aokenk">okenk</a>
</td>
</tr>

</table>


### Issue Contributors

<table>

<tr>
<td align="center">
<a href="https://github.com/James-Thorson-NOAA">
<img src="https://avatars.githubusercontent.com/u/50178738?u=d314fbca8b41fdeb14212782d70b156a14e34af3&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/issues?q=is%3Aissue+commenter%3AJames-Thorson-NOAA">James-Thorson-NOAA</a>
</td>
<td align="center">
<a href="https://github.com/brianlangseth-NOAA">
<img src="https://avatars.githubusercontent.com/u/27824606?u=92161ed2e44b80a4929d389ee4ca5356b28d815e&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/pfmc-assessments/indexwc/issues?q=is%3Aissue+commenter%3Abrianlangseth-NOAA">brianlangseth-NOAA</a>
</td>
</tr>

</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->

