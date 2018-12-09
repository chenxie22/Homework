* Compute (n-1) times univariate regression coefficients for mpg vs 
* other continuous variables by cylinder(cyl) groups. 

// Read data and reduce to needed varaibales
import delimited mtcars.csv

local vars = "disp hp wt" // mpg will be regressed against these by cyl
keep mpg cyl `vars'

// Sort by cyl and then compute centered variables, 
// cross product with mpg, and squares. 
gsort+ cyl

foreach var in `vars' {
 by cyl: egen `var'_grp_mean = mean(`var')
 generate `var'_gc = `var' - `var'_grp_mean
 generate `var'Xmpg = mpg * `var'_gc
 generate `var'_sq = `var'_gc * `var'_gc
}

// Compute the cross products, sum of squares, and (n-1)
// (n-1)*regression coefficients
collapse (sum) *Xmpg (sum) *_sq, by(cyl)

// To make thes accurate beta hats, there should also be scaling factor 
// of 1/{n-1}.

quietly describe
local n=r(N)

foreach var in `vars' {
 generate beta_cyl_`var' = `var'Xmpg / `var'_sq 
}

drop *Xmpg *_sq

// Export values
export delimited mpg_betas_by_cyl.csv, replace
