module Help (help) where

import           Input

-- It is assumed that help will only be called if the first arg is a help request
help :: [String] -> String
help [_        ] = defaultHelp
help (_ : x : _) = case lookup (take 3 x) argumentHelp of
   Nothing  -> error $ "Invalid argument \"" ++ x ++ "\""
   (Just y) -> y


{-- 
************************************
TODO ADD HELP FOR GRID VISUALIZATION
************************************
--} 


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
 "           |zoom fnl frms (Optional: fIter)                                                   \n" ++
 "           |iterations fnl frms                                                               \n" ++
 "           |theta fnl frms (Optional: fMag)                                                   \n" ++
 "           |linearC fnl frms                                                                  \n" ++
 "                                                                                              \n" ++
 "-cvalue  |rectangular real imag  The value c used for creation of a Julia fractal             \n" ++
 "         |polar mag phase                                                                     \n" ++
 "                                                                                              \n" ++
 "-setcolor r,g,b                  The color used for points within the set                     \n" ++
 "                                                                                              \n" ++
 "-startingframe f                 The frame to begin on                                        \n" ++
 "                                                                                              \n" ++
 "Only the first 3 characters (including dashes) of any argument are required.                  \n"

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
       "   and use the point's complex value for the initial z-value.                           \n"
     )
   , ( "-re"
     , "*** Mayge's Fractal Generator :: Help :: -resolution ***                                \n" ++
       "-resolution (int horiz) (int vert)                                                      \n" ++
       "   Sets the resolution, in pixels, of the output images.                                \n"
     )
   , ( "-ce"
     , "*** Mayge's Fractal Generator :: Help :: -center ***                                    \n" ++
       "-center rectangular (float real) (float imag)                                           \n" ++
       "   Sets the center of the fractal in rectanguar coordinates by defining the center's    \n" ++
       "   real (x) and imaginary (y) components.                                               \n" ++
       "-center polar (float magnitude) (float phase (degrees))                                 \n" ++
       "   Sets the center of the fractal in polar coordinates by defining the center's         \n" ++
       "   magnitude (distance from the origin) and phase (angle).                              \n"
     )
   , ( "-ra"
     , "*** Mayge's Fractal Generator :: Help :: -range ***                                     \n" ++
       "-range (float range)                                                                    \n" ++
       "   Sets the horizontal distance from the center along the real number line that the     \n" ++
       "   image will cover.                                                                    \n"
     )
   , ( "-it"
     , "*** Mayge's Fractal Generator :: Help :: -iterations ***                                \n" ++
       " -iterations (int iterations)                                                           \n" ++
       "    Sets the maximum number of iterations that are tested on a pixel before it is       \n" ++
       "    considered to be in the set.                                                        \n"
     )
   , ( "-po"
     , "*** Mayge's Fractal Generator :: Help :: -power ***                                     \n" ++
       "-power (float power)                                                                    \n" ++
       "   Sets the power (d) used in the fractal equation z(n+1) = z(n)^d + c.                 \n"
     )
   , ( "-aa"
     , "*** Mayge's Fractal Generator :: Help :: -aa ***                                        \n" ++
       "-aa enable                                                                              \n" ++
       "   Enable supersample antialiasing, which works by generating 9 subpixels within eac    \n" ++
       "   pixel and averaging their results. Generates significantly nicer images, but at a    \n" ++
       "   significant performance cost.                                                        \n"
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
       "   Normalizes iteration counts along a sine wave with the specified period.             \n"
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
       "   Same as above, but the gradient does not wrap back to the first value.               \n"
     )
   , ( "-an"
     , "*** Mayge's Fractal Generator :: Help :: -animation ***                                 \n" ++
       "-animation none                                                                         \n" ++
       "   Generates a single frame.                                                            \n" ++
       "-animation power (float final) (int frames)                                             \n" ++
       "   Generates an animation which varies the power used in the iteration equation.        \n" ++
       "-animation zoom (float final) (int frames) (Optional: int finalIterations)              \n" ++
       "   Generates an animation which zooms into the fractal to the specified final range,    \n" ++
       "   while also varying the maximum iteration count (as increasingly zoomed in views      \n" ++
       "   require more iterations to reveal all their detail.                                  \n" ++
       "-animation iterations (int final) (int frames)                                          \n" ++
       "   Generates an animation which varies the maximum number of iterations.                \n" ++
       "-animation theta (float final) (int frames) (Optional: float finalMagnitude)*JULIA ONLY*\n" ++
       "   Generates an animation by varying the theta value (aka phase or angle) of the c-value\n" ++
       "   used to generate a Julia set.                                                        \n" ++
       "-animation linear (complex final) (int frames)  *JULIA ONLY*                            \n" ++
       "   Generates an animation by varying the c-value used to generate a Julia set linearly. \n" ++
       "   The final point should be entered in the same way as avalue is entered, in either    \n" ++
       "   polar or rectangular form.                                                           \n"
     )
   , ( "-cv"
     , "*** Mayge's Fractal Generator :: Help :: -cvalue ***   *JULIA ONLY*                     \n" ++
       "-cvalue rectangular (float real) (float imag)                                           \n" ++
       "   Sets the c-value of the fractal in rectanguar coordinates by defining the c-value's  \n" ++
       "   real (x) and imaginary (y) components.                                               \n" ++
       "-cvalue polar (float magnitude) (float phase (degrees))                                 \n" ++
       "   Sets the c-value of the fractal in polar coordinates by defining the c-value's       \n" ++
       "   magnitude (distance from the origin) and phase (angle).                              \n"
     )
   , ( "-se"
     , "*** Mayge's Fractal Generator :: Help :: -setcolor ***                                  \n" ++
       "-setcolor (int r1),(int g1),(int b1)                                                    \n" ++
       "   Defines the color that should be used for pixels that are found to be in the set.    \n"
     )
   , ( "-st"
     , "*** Mayge's Fractal Generator :: Help :: -startingframe ***                             \n" ++
       "-startingframe (int frame)                                                              \n" ++
       "   Defines the frame to start the animation on. Useful for resuming animations that were\n" ++
       "   interrupted by a power update or unexpected OS update.                               \n" ++
       "                                                                                        \n"
     )
   ]