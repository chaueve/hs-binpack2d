# binpack2d

Provides a bin packing algorithm. The algorithm used is bottom left skyline
algorithm from which all wasted space is fed to a modified guillotine
algorithm. The modification allows the guillotine to combine adjacent
cells of the same base and height.

Originally created to make texture atlases, but useful for any situation
where you need to pack axis aligned rectangles with integer dimensions to
another (bigger) axis aligned rectangle with integer dimensions.
