test_that(" remove if necessary ",{
expect_identical(cleanFactor( 1:5, factor(c('A', 'A', 'B', 'B','C')))$x, 1:4 )

# Remove constant level 
x <- c( 1, 1:6)
g <- factor(c(rep('A',2), rep('B',3), rep('C', 2)))
expect_equal(cleanFactor(x,g)$x, 2:6 )

#Error because B is the only level
x <- c( 1, 1:5)
g <- factor(c(rep('A',2), rep('B',3), rep('C', 1)))
expect_error(cleanFactor(x,g)$x )

})