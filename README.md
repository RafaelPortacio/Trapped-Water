# Trapped-Water
A function in Racket( a functional language from the LISP family) that estimate the amount of water a surface can contain after a rain. (Doesn't considers molecular interations effects such as surface tension of the liquid)

The function receives a matrix where every element represents a position on the surface and the respective value represents the height in that position. The algorithm assumes that there is no barricades around the surface.

For example, the simple matrix below of integers:

        [ 2 3 4 5 ]
    X = [ 8 4 1 3 ]
        [ 6 2 1 6 ]
        [ 9 8 7 6 ]
        
*OBS:* The values doesn't have to be integers.
After the rain could contain water and if we consider the water columns as additional height we would have something just like this:

        [ 2 3 4 5 ]
    X = [ 8 4 3 3 ]
        [ 6 3 3 6 ]
        [ 9 8 7 6 ]
        
Some positions in the middle filled up to the height of 3, because it's the maximum height of water it can contain, if it contained more it would escape through the opening to the outside in the 3 in the position 2x4. Depending on the matrix you can have differents portions of water around the surface.

The function estimates the total amount of water by calculating the difference between the sum of the heights of final and the initial matrices (in the example above, we would have 5). So with that value in hands, you can estimate the amount of water contained by considering how much area the elements of the matrix have in your experiment. And the bigger the matrix you give to describe your surface, the more adjusted to the surface will be the heights and the more accurate will be your answer.
