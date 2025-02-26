version 13
* set the number of ri replications
global reps 1000

/* Sets stata globals */
global raw = "../data"
global int = "../data"

if "`c(username)'" == "charlierafkin" {
  global raw = "~/Dropbox (MIT)/COVID/data/raw_decrypted/"
  global int = "~/Dropbox (MIT)/COVID/data/intermediate/"
}
if "`c(username)'" == "pierre-lucvautrey" {
  global raw = "~/Dropbox (MIT)/COVID/data/raw_decrypted/"
  global int = "~/Dropbox (MIT)/COVID/data/intermediate/"
}

global navy `" "51 122 183" "'
global green `" "92 184 92" "'
global ltblue `" "91 192 222" "'
global red `" "217 83 79" "'
global orange `" "240 173 78" "'

qui {
/*

Convert eps to pdf in mac Stata
Author: CR
Date: April 2018

*/


capture pr drop conversion
capture pr define conversion
syntax, name(string)

        !epstopdf `name'.eps
        !rm -f `name'.eps
        di "`name' written as PDF"



end

capture pr drop grout
capture pr define grout
syntax anything, [png pdf]

/* error */
if "`png'" != "" & "`pdf'" != "" {
    noisily di "cannot invoke both pdf and png"
    stop
}

/* default export is to pdf */
if "`png'" == "" & "`pdf'" == "" {
    local pdf = "pdf"
}

qui {
        if "`pdf'" != "" {
            gr export `1'.eps, replace
            !epstopdf `1'.eps
            !rm -f `1'.eps
            noisily di "`1' written as PDF"
        }
        if "`png'" != "" {
            gr export `1'.eps, replace
            !convert -density 500 -resize '800' `1'.eps `1'.png
            !rm -f `1'.eps
            noisily di "`1' written as PNG"
        }
}
end

/******************************************/
/* /\***** deflate cpi, base year *****\/ */
/******************************************/
capture pr drop get_cpi_deflator
capture pr define get_cpi_deflator
syntax, baseyear(int) [mergekey(string) replace cpicsv(string)]
qui {
    /* error checks */
    if inrange(`baseyear',1947,2019) != 1 {
        di "base year must be 1947--2019"
        stop
    }

    /* if the csv holding cpi isn't specified, you find it in jenna's bulk */
    if "`cpicsv'" == "" {
        local cpicsv = "/homes/nber/andersj/bulk/AndersRafkin/stigma/raw/general/CPIAUCSL.csv"
    }

    /* if mergekey isn't specified, the name is year */
    if "`mergekey'" == "" {
        local mergekey = "year"
    }

    /* if replace is on, you re-import CPI */
    if "`replace'" != "" {
        preserve
            import delimited using `cpicsv', clear
            gen year = real(substr(date,1,4))
            collapse (mean) cpi, by(year)
            ren cpiaucsl cpi
            save $intermed/cpi, replace
        restore
    }

    /* gets deflator by base year */
    preserve
        use $intermed/cpi, clear
        sum cpi if year == `baseyear'
        replace cpi = cpi/`r(mean)'
        tempfile cpi
        save `cpi'
    restore

    /* merges */
    if "`assertoff'" == "" {
        merge m:1 `mergekey' using `cpi', assert(using match)  nogen keepusing(cpi) keep(master match)
    }
    else {
        merge m:1 `mergekey' using `cpi'
    }
}
end







/* other graphing/output programs that we want */

/*************************************************************************************/
/* program store_est_tpl : store b, se, p-values in format suitable for table_to_tpl */
/*************************************************************************************/
cap prog drop store_est_tpl
prog def store_est_tpl
{
  syntax using/,  coef(string) name(string) [format(string) beta se p n r2 all]

  /* turn on all flags if `all' is specified */
  if !mi("`all'") {
    local beta beta
    local se se
    local p p
    local n n
    local r2 r2
  }

  /* set default format if not specified */
  if mi("`format'") local format "%6.3f"

  /* manage beta */
  if !mi("`beta'") {

    /* store beta in the desired format */
    local b3:  di `format' _b["`coef'"]
    test `coef' = 0
    local pvalue = `r(p)'
    /* write p value (only makes sense if beta also specified) */
    if !mi("`p'") {
      insert_into_file using `using', key("`name'_p") value(" `pvalue'") format("%5.2f")
    }

    /* write beta to file */
    insert_into_file using `using', key("`name'_beta") value(" `b3'") format("`format'")

    /* count stars on the p and create starbeta */
    count_stars, p(`pvalue')
    insert_into_file using `using', key("`name'_starbeta") value(" `b3'`r(stars)'") format("`format'")
  }

  /* manage se */
  if !mi("`se'") {
    local se3:  di `format' _se["`coef'"]
    insert_into_file using `using', key("`name'_se") value(" `se3'")
  }

  /* manage n */
  if !mi("`n'") {
    insert_into_file using `using', key("`name'_n") value(" `e(N)'") format("%1.0f")
  }

  /* manage r2 */
  if !mi("`r2'") {
    insert_into_file using `using', key("`name'_r2") value(" `e(r2)'") format("%5.2f")
  }
}
end
/* *********** END program store_est_tpl ***************************************** */

/*************************************************************************************/
/* program store_val_tpl : store value to a file with string and format              */
/*************************************************************************************/
cap prog drop store_val_tpl
prog def store_val_tpl
{
  syntax using/,  Name(string) Value(string) [Format(string)]

  di `""store_val_tpl, name value format" is deprecated -- please use "insert_into_file, key value format" instead"'

  /* set default format if not specified */
  if mi("`format'") local format "%6.3f"

  /* get value in appropriate format (unless it's already a string) */
  if !mi(real("`value'")) {
    local v : di `format' `value'
  }
  else {
    local v `value'
  }

  /* write line to file */
  append_to_file using `using', s("`name',`v'")
}
end
/* *********** END program store_val_tpl ***************************************** */

/***************************************************************************************************/
/* program table_from_tpl : Create a table from a stored estimates file and a .tex table template  */
/***************************************************************************************************/
cap prog drop table_from_tpl
prog def table_from_tpl
{
  syntax, Template(string) Replacement(string) Output(string) [Verbose addstars dropstars]

  /* set up verbose flag */
  if !mi("`verbose'") {
      local v "-v"
  }
  else {
      local v
  }


  /* if python path is not set, use current folder */
  if mi("$PYTHONPATH") {

      /* set path to current folder */
      local path .
  }
  else {
      local path $PYTHONPATH
  }

  /* check python file existence */
  cap confirm file `path'/table_from_tpl.py
  if _rc {
      display as error "ERROR: table_from_tpl.py not found. Put in current folder or folder defined by global \$PYTHONPATH"
      error -1
  }

  /* deal with addstars/dropstars parameters */
  if "`addstars'" == "addstars" {
    local star_param "--add-stars"
  }
  if "`dropstars'" == "dropstars" {
    local star_param "--drop-stars"
  }

  local pycommand `path'/table_from_tpl.py -t `template' -r `replacement' -o `output' `v' `star_param'
  if !mi("`verbose'") {
      di `"Running `pycommand' "'
  }

  shell python `pycommand'
  cap confirm file `output'
  if !_rc {
    display "Created `output'."
  }
  else {
    display "Could not create `output'."
    error 1
  }

  /* clean up the temporary file if star/nostar specified */
  if !mi("`stars'") {
    !rm $tmp/tpl_sed_tmp.tex
  }
}
end
/* *********** END program table_from_tpl ***************************************** */

/**********************************************************************************/
/* program append_to_file : Append a passed in string to a file                   */
/**********************************************************************************/
cap prog drop append_to_file
prog def append_to_file
{
  syntax using/, String(string)

  cap file close fh
  file open fh using `using', write append
  file write fh  `"`string'"'  _n
  file close fh
}
end
/* *********** END program append_to_file ***************************************** */

/**********************************************************************************/
/* program insert_into_file : Insert a key-value pair into a file                 */
/*
Assume "using" csv file take a key, value format, e.g.:

est1,3.544
est2,3.234***
...

"est1" is the key. "3.544" is the value.

Example:

insert_into_file using $tmp/estimates.csv, key(est1) value(3.54493) format(%5.2f)

- if "est1" is not already in estimates file, it will be appended
- if "est1" is already in estimates file, its value will be replaced with the passed in parameter
- estimates file will be created if it does not already exist

*/

/***********************************************************************************/
cap prog drop insert_into_file
prog def insert_into_file
{
  syntax using/, Key(string) Value(string) [Format(string) verbose]

  /* set default format if not specified */
  if mi("`format'") local format "%6.3f"

  /* get value in correct format (unless it's a string) */
  if !mi(real("`value'")) {
    local value : di `format' `value'
  }
  else {
    local value `value'
  }

  /* confirm file handles are closed */
  cap file close fout
  cap file close fin

  /* create a temporary file for writing */
  tempfile tempfile
  qui file open fout using `tempfile', write replace

  /* if input file doesn't exist, create it and display a notification */
  cap confirm file `using'
  if _rc {
    if !mi("`verbose'") {
      di "Creating new file `using'..."
    }
  }

  /* else, open the input file and read the first line */
  else {
    file open fin using `using', read

    /* read the first line */
    file read fin line
  }

  /* store a flag indicating whether we found the line or not */
  local found 0

  /* loop over all lines of the file */
  while r(eof) == 0 {

    /* check if line matches the current key */
    if regexm("`line'", "^`key',") {

      /* if verbose, show what we're replacing  */
      if !mi("`verbose'") {
        di `"Replacing "`line'" with "`key',`value'"..."'
      }
      local found 1

      /* replace the line with key,value */
      local line `key',`value'
    }

    /* write the line to the output file */
    file write fout "`line'" _n

    /* read the next line */
    file read fin line
  }

  /* if we didn't find this key, append it to the end */
  if `found' == 0 {
    file write fout "`key',`value'" _n
  }

  /* close input and output files */
  cap file close fin
  file close fout

  /* copy the temporary file to the `using` filename */
  copy `tempfile' `using', replace
}
end
/* *********** END program insert_into_file ***************************************** */

/**********************************************************************************/
/* program count_stars : return a string with the right number of stars           */
/**********************************************************************************/
cap prog drop count_stars
prog def count_stars, rclass
{
  syntax, p(real)
  local star = ""
  if `p' <= 0.1  local star = "*"
  if `p' <= 0.05 local star = "**"
  if `p' <= 0.01 local star = "***"
  return local stars = "`star'"
}
end
/* *********** END program count_stars ***************************************** */

}

if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/COVID/programs.do
}
else{
  do ../programs.do
}
global PYTHONPATH = "code"
