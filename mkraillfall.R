library(vetools)
setwd("~/ownCloud/paper-estaciones/precip-clean-desi")

k = 1
df = data.frame(matrix(NA, ncol=13, nrow=3*24), stringsAsFactors = FALSE)
colnames(df) <- c('state', month.name)

# Make dataset ####
for (ES in get.shape.state()$Abb) {
        load(paste0('precip-', ES, '.rda'))
        # plot(pr.med.AM)
        eval(parse(text = paste0("data <- pr.med.", ES)))
        s = window(data, start = c(1995, 1), end=c(2005, 12))
        # plot(s)
        # time(s)
        n = length(s) %% 12
        if ( n != 0 ) { s = c(s, rep(NA, 12 - n)) }
        SS = matrix(s, ncol=12, byrow = TRUE)
        sm = apply(SS, MARGIN = 2, FUN = function(x) {c(median(x, na.rm = TRUE), mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))} )
        rownames(sm) <- c('median', 'mean', 'sd')
        colnames(sm) <- month.name
        # print(ceiling(sm))
        df[k:(k+2), 1] <- rep(ES, 3)
        df[k:(k+2), 2:13] <- sm
        k = k + 3
}

# Save dataset ####
View(df)
df.r = df
df.r[, 2 : 13] = ceiling(df[, 2 : 13])
save(file = 'mapdatave.rda', list = c('df.r', 'df'))

# Build maps ####
load(file = 'mapdatave.rda')
MAX = max(max(df.r[seq(1, (3*24), by=3), 2 : 13]))
MAXSD = max(max(df.r[seq(3, (3*24), by=3), 2 : 13]))
pr.ramp <- function(x) { rgb( colorRamp(c('goldenrod1', 'deepskyblue4'))(x)/255 )}
prsd.ramp <- function(x) { rgb( colorRamp(c('darkseagreen1', 'red2'))(x)/255 )}
prrel.ramp <- function(x) { rgb( colorRamp(c('greenyellow', 'lightblue4'))(x)/255 )}
VE = get.shape.venezuela()
pdf(file='rainfall1995-2005.pdf', width=8, height=8)
par(mar=c(0,0,1,0), oma=c(0,0,0,0))
# Absolute nation-wide ####
for (i in 1 : 12) {
        plot(VE, col = NA)
        title(main = month.name[i]) #, sub='Absolute nation-wide')
        k = 1
        for ( ES in get.shape.state()$Abb ) {
                SH <- get.shape.state(ES)
                med = df.r[k, 2 : 13] / MAX
                plot(SH, col = pr.ramp(med[i]), add = TRUE)
                k = k + 3
        }
}
for (i in 1 : 12) {
        plot(VE, col = NA)
        # title(main = month.name[i], sub='Std. Dev.')
        title(main = month.name[i]) #, sub='Absolute nation-wide')
        k = 3
        for ( ES in get.shape.state()$Abb ) {
                SH <- get.shape.state(ES)
                med = df.r[k, 2 : 13] / MAXSD
                plot(SH, col = prsd.ramp(med[i]), add = TRUE)
                k = k + 3
        }
}


# Relative state-wise ####
for (i in 1 : 12) {
        plot(VE, col = NA)
        # title(main = month.name[i], sub='Relative rainfall')
        title(main = month.name[i]) #, sub='Absolute nation-wide')
        k = 1
        for ( ES in get.shape.state()$Abb ) {
                SH <- get.shape.state(ES)
                MAXR = max(df.r[k, 2 : 13])
                med = df.r[k, 2 : 13] / MAXR
                plot(SH, col = prrel.ramp(med[i]), add = TRUE)
                k = k + 3
        }

}
dev.off()

source('scalebars.R')
n = seq(0, 1, length.out = 10)
pdf(file='scales.pdf', width=8, height=1.25)
print_ramp_h(n, pr.ramp(n), axes=FALSE)
axis(1, at = c(0, 0.5, 1), labels = paste(c(0, MAX/2, MAX), 'mm'))
print_ramp_h(n, prsd.ramp(n), axes=FALSE)
axis(1, at = c(0, 0.5, 1), labels = paste(c(0, MAXSD/2, MAXSD), 'mm'))
print_ramp_h(n, prrel.ramp(n), axes=FALSE)
axis(1, at = c(0, 1), labels = c('Dry', 'Rainy'))
dev.off()
