`.onLoad` <-
function(libname, pkgname) {
}

`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('packagePages',paste(paste0(unlist(strsplit(as.character(packageVersion("packagePages")), "[.]")), c(".", "-", ".", "")), collapse=""),' (4-15-2021). For help: >help("packagePages") or visit https://centerforassessment.github.io/packagePages'))
	}
}
