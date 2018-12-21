module Help (help) where

import           Input

-- It is assumed that help will only be called if the first arg is a help request
help :: [String] -> String
help [_        ] = defaultHelp
help (_ : x : _) = case lookup (take 3 x) argumentHelp of
   Nothing  -> error $ "Invalid argument \"" ++ x ++ "\""
   (Just y) -> y


-- The text that gets printed if the user asks for help in their arguments
defaultHelp :: String
defaultHelp = 
 "*** Mayge's Fractal Generator :: Help :: General ***                                          \n" ++
 "                                                                                              \n" ++
 "By default, this program generates a fractal with the following attributes:                   \n" ++
 pPrintOptions defaultOptions                                                                       ++
 "                                                                                              \n" ++
 "These defaults can be changed through a series of command line arguments:                     \n" ++
 "                                                                                              \n" ++
 "-fractal |mandelbrot             The type of fractal to be generated                          \n" ++
 "         |julia                                                                               \n" ++
 "                                                                                              \n" ++
 "-resolution horiz vert           The resolution of your images                                \n" ++
 "                                                                                              \n" ++
 "-center |rectangular real imag   The complex center of your images                            \n" ++
 "        |polar mag phase                                                                      \n" ++
 "                                                                                              \n" ++
 "-range r                         The real distance from center to the edge of your picture    \n" ++
 "                                                                                              \n" ++
 "-iterations i                    The maximum number of times a point should be iterated       \n" ++
 "                                                                                              \n" ++
 "-power p                         The power used in the iteration equation                     \n" ++
 "                                                                                              \n" ++
 "-aa |enabled                     Whether subsampling antialiasing (9 subpixels) is enabled    \n" ++
 "    |disabled                                                                                 \n" ++
 "                                                                                              \n" ++
 "-normalization |linear           How to map iteration counts to [0,1] for coloring            \n" ++
 "               |sigmoid mid pwr                                                               \n" ++
 "               |periodic period                                                               \n" ++
 "               |sine period                                                                   \n" ++
 "                                                                                              \n" ++
 "-color |greyscale                The color scheme used to generate the images                 \n" ++
 "       |hue                                                                                   \n" ++
 "       |gradient [r,g,b]                                                                      \n" ++
 "       |gradient linear [r,g,b]                                                               \n" ++
 "                                                                                              \n" ++
 "-animation |none                 The animation that should be generated                       \n" ++
 "           |power fnl frms                                                                    \n" ++
 "           |zoom fnl frms fIter                                                               \n" ++
 "           |iterations fnl frms                                                               \n" ++
 "           |theta fnl frms                                                                    \n" ++
 "           |linearC fnl frms                                                                \n" ++
 "                                                                                              \n" ++
 "-cvalue  |rectangular real imag  The point c used for ccreation of a Julia fractal            \n" ++
 "         |polar mag phase                                                                     \n" ++
 "                                                                                              \n" ++
 "-setcolor r,g,b                  The color used for points within the set                     \n" ++
 "                                                                                              \n" ++
 "-startingframe f                 The frame to begin on                                        \n" ++
 "                                                                                              \n" ++
 "Only the first 3 characters (including dashes) of any argument are required.                  "

-- List which pairs arguments with their help text
argumentHelp :: [(String, String)]
argumentHelp =
   [ ( "-fr"
     , "*** Mayge's Fractal Generator :: Help :: -fractal ***                                   \n" ++
       "-fractal mandelbrot                                                                     \n" ++
       "   Generates a Mandelbrot Set fractal, where each point's c-value and initial z-valu    \n" ++
       "   are both equal to the point's complex value.                                         \n" ++
       "-fractal julia                                                                          \n" ++
       "   Generates a Julia Set fractal, where all points use the same predetermined c-valu    \n" ++
       "   and use the point's complex value for the initial z-value.                           "
     )
   , ( "-re"
     , "*** Mayge's Fractal Generator :: Help :: -resolution ***                                \n" ++
       "-resolution (int horiz) (int vert)                                                      \n" ++
       "   Sets the resolution, in pixels, of the output images.                                "
     )
   , ( "-ce"
     , "*** Mayge's Fractal Generator :: Help :: -center ***                                    \n" ++
       "-center rectangular (float real) (float imag)                                           \n" ++
       "   Sets the center of the fractal in rectanguar coordinates by defining the center's    \n" ++
       "   real (x) and imaginary (y) components.                                               \n" ++
       "-center polar (float magnitude) (float phase (degrees))                                 \n" ++
       "   Sets the center of the fractal in polar coordinates by defining the center's         \n" ++
       "   magnitude (distance from the origin) and phase (angle).                              "
     )
   , ( "-ra"
     , "*** Mayge's Fractal Generator :: Help :: -range ***                                     \n" ++
       "-range (float range)                                                                    \n" ++
       "   Sets the horizontal distance from the center along the real number line that the     \n" ++
       "   image will cover.                                                                    "
     )
   , ( "-it"
     , "*** Mayge's Fractal Generator :: Help :: -iterations ***                                \n" ++
       " -iterations (int iterations)                                                           \n" ++
       "    Sets the maximum number of iterations that are tested on a pixel before it is       \n" ++
       "    considered to be in the set.                                                        "
     )
   , ( "-po"
     , "*** Mayge's Fractal Generator :: Help :: -power ***                                     \n" ++
       "-power (float power)                                                                    \n" ++
       "   Sets the power (d) used in the fractal equation z(n+1) = z(n)^d + c.                 "
     )
   , ( "-aa"
     , "*** Mayge's Fractal Generator :: Help :: -aa ***                                        \n" ++
       "-aa enable                                                                              \n" ++
       "   Enable supersample antialiasing, which works by generating 9 subpixels within eac    \n" ++
       "   pixel and averaging their results. Generates significantly nicer images, but at a    \n" ++
       "   significant performance cost.                                                        "
     )
   , ( "-no"
     , "*** Mayge's Fractal Generator :: Help :: -normalization ***                             \n" ++
       "Before color can be calculated, all iteration counts have to be mapped to the range     \n" ++
       "[0,1], which can be accomplished in several ways.                                       \n" ++
       "-normalization linear                                                                   \n" ++
       "   Normalizes the values in a linear manner, where an iteration count of 0 becomes 0    \n" ++
       "   and an iteration count equal to the maximum iteration count becomes 1.               \n" ++
       "-normalization sigmoid (float center) (float power)                                     \n" ++
       "   Uses a sigmoid function to normalize the values, which causes no repeated colors.    \n" ++
       "-normalization periodic (float period)                                                  \n" ++
       "   Normalizes iteration counts periodically using the formula (x % period) / period.    \n" ++
       "-normalization sine (float period)                                                      \n" ++
       "   Normalizes iteration counts along a sine wave with the specified period.             "
     )
   , ( "-co"
     , "*** Mayge's Fractal Generator :: Help :: -color ***                                     \n" ++
       "-color greyscale                                                                        \n" ++
       "   Color function where 0 is pure black and 1 is pure white. Good with sine norm.       \n" ++
       "-color hue                                                                              \n" ++
       "   Color function which maps through the entire hue spectrum. Good with periodic norm.  \n" ++
       "-color gradient (int r1),(int g1),(int b1) (int r2),(int g2),(int b2) ...               \n" ++
       "   Color funtion which maps to a gradient of user-defined values. The gradient wraps    \n" ++
       "   back to the first value at the end of the period.                                    \n" ++
       "-color gradient l (int r1),(int g1),(int b1) (int r2),(int g2),(int b2) ...             \n" ++
       "   Same as above, but the gradient does not wrap back to the first value.               "
     )
   , ( "-an"
     , "*** Mayge's Fractal Generator :: Help :: -animation ***                                 \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        "
     )
   , ( "-cv"
     , "*** Mayge's Fractal Generator :: Help :: -cvalue ***                                    \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        "
     )
   , ( "-se"
     , "*** Mayge's Fractal Generator :: Help :: -setcolor ***                                  \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        "
     )
   , ( "-st"
     , "*** Mayge's Fractal Generator :: Help :: -startingframe ***                             \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        \n" ++
       "                                                                                        "
     )
   ]