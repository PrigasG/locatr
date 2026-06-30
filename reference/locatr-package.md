# locatr: Audit-Ready Geocoding and Local Geography for Messy Location Data

Cleans, geocodes, validates, reviews, and exports messy location address
data supplied by the user. The package sits on top of 'tidygeocoder': it
calls geocoding services, rejects implausible coordinates with
configurable region guards, applies fallback name/address matching,
joins points to optional local geography with 'sf', and records an audit
trail showing how each coordinate was produced. Outputs are designed for
manual review, dashboards, and reusable location crosswalks.

## See also

Useful links:

- <https://prigasg.github.io/locatr/>

- <https://github.com/PrigasG/locatr>

- Report bugs at <https://github.com/PrigasG/locatr/issues>

## Author

**Maintainer**: George Arthur <prigasgenthian48@gmail.com>
([ORCID](https://orcid.org/0000-0002-1975-1459))
