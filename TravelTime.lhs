\documentclass{article}
\usepackage{amsmath}

\usepackage{fancyvrb}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\DefineVerbatimEnvironment{example}{Verbatim}{fontsize=\small}
\newcommand{\ignore}[1]{}

\newcommand\vmax{\ensuremath{V_{\text{max}}}}
\newcommand\paren[1]{\left({#1}\right)}
\begin{document}
The purpose of this module is to model the amount of time $t$ it
requires to travel a certain distance $X$ starting and ending at rest.
Assuming uniform acceleration and braking, a vehicle is described by
three values: the maximum velocity $V_{max}$, the constant
acceleration $A$ applied to put the vehicle in motion, and the
constant deceleration $B$ used to bring it to a halt.  There are up to
three phases to the journey: the time $t_A$ it takes to accelerate,
the time $t_B$ it takes to decelerate, and the time $t_C$ spent
cruising at a constant velocity.  Each of these phases corresponds to
a distance travelled: $X_A, X_B, X_C$.  The problem can then be stated
as: ``How much time $t=t_A+t_B+t_C$ does it take to travel distance
$X=X_A+X_B+X_C$?''

\begin{code}
module TravelTime
  ( timeToTravel, timeTriple, Vehicle (..), xmaxNoCruise )
where

class Vehicle v where
  vehicleAccel, vehicleBrake, vehicleMaxSpeed :: v -> Double

-- convenience
accel, brake, vmax :: Vehicle v => v -> Double
accel = vehicleAccel; brake = vehicleBrake; vmax = vehicleMaxSpeed

\end{code}

The first thing to observe is that with a maximum velocity, then there
must be a maximum amount of time spent accelerating.
\begin{flalign*}
  t_A &\leq \frac\vmax{A}  
\end{flalign*}
\begin{code}
tAmax veh = vmax veh / accel veh

\end{code}
Next, the time spent braking is dependent on the time spent
accelerating, because the latter determines the cruising velocity, and
$t_B$ is the amount of time required to reduce the velocity from that
to zero.  I'll call this function $\beta(t_A)$ for convenience.
\begin{flalign*}
  At_A + Bt_B &= 0\\
  t_B &= -\frac A B t_A\\
  t_B &= \beta(t_A)
\end{flalign*}
\begin{code}
beta veh tA = - accel veh * tA / brake veh

\end{code}

The distance travelled during acceleration is the second
antiderivative of $A$, namely $X_A(t)=\frac 1 2 At^2$.  The braking
distance is similar but one of the constants introduced during
antiderivation is non-zero (namely the cruising velocity), therefore
$X_B(t)=\frac 1 2 Bt^2 + V_{B_0}t$ and we know that $V_{B_0}=At_A$.
The cruising distance is simply $X_C(t)=At_At$.

Then a function for $X(t)$ can be assembled, based on the distance
travelled during acceleration, during cruising, and during braking.
\begin{flalign*}
  X(t_A,t_B,t_C) &= \frac 1 2 At_A^2 + At_At_C + \frac 1 2 Bt_B^2 + At_At_B
\end{flalign*}
and using the facts above
\begin{flalign*}
  X(t_A,t_C) &= \frac 1 2 At_A^2 + At_At_C + \frac 1 2 B\paren{-\frac A B t_A}^2 + At_A\paren{-\frac A B t_A} \\
  &= \frac{t_A^2} {2B}\paren{AB - A^2} + At_At_C
\end{flalign*}
This can be solved in terms of $t_A$ or $t_C$, but let us make a
simplifying assumption first: the vehicle will always accelerate to
maximum velocity before cruising; if there is not enough space to
reach maximum velocity, then there will be no cruising time.

\paragraph{Case 1} If $t_C=0$ then $X$ is limited by \vmax.
\begin{flalign*}
  X(t_A,0) &= \frac{t_A^2}{2B}\paren{AB-A^2}\\
  t_A &\leq \frac\vmax{A} \\
  X(t_A,0) &\leq X\paren{\frac\vmax A, 0} \\
  &\leq \frac {\vmax^2} 2\paren{\frac 1 A - \frac 1 B}
\end{flalign*}
\begin{code}
xmaxNoCruise veh = vmax veh**2 / 2 * (1/a - 1/b)
  where a = accel veh; b = brake veh

\end{code}
assuming that $X$ falls within this bound, then
\begin{flalign*}
  t_A &= \sqrt{\frac {2BX} {AB-A^2}}
\end{flalign*}

\paragraph{Case 2} If $t_C>0$ then $t_A=\frac\vmax A$.
\begin{flalign*}
  X\paren{\frac\vmax A, t_C} &= \frac {\vmax^2} 2 \paren{\frac 1 A - \frac 1 B} + \vmax t_C 
\end{flalign*}
solving for $t_C$ in terms of $X$ then becomes
\begin{flalign*}
  t_C &= \frac 1 \vmax \paren {X - \frac {\vmax^2} 2 \paren {\frac 1 A - \frac 1 B}}
\end{flalign*}

An overall $t(X)$ can be constructed by cases
\begin{flalign*}
  t(X) &=
  \begin{cases}
    T_A (X) - \frac A B T_A (X) & \text{if } X\leq\frac {\vmax^2} 2\paren{\frac 1 A - \frac 1 B}\\
    \frac \vmax A - \frac \vmax B + T_C (X) & \text{otherwise}
  \end{cases}
\end{flalign*}
\begin{code}
timeTriple veh x
  | x <= xmax = (tA, beta veh tA, 0)
  | otherwise = (vmax veh / a, beta veh (vmax veh / a), tC)
  where
    a = accel veh; b = brake veh
    xmax = xmaxNoCruise veh

\end{code}
where the functions computing $t_A, t_C$ in terms of $X$ are
\begin{flalign*}
  T_A (X) &= \sqrt{\frac{2BX}{AB-A^2}} \\
  T_C (X) &= \frac 1 \vmax \paren {X - \frac {\vmax^2} 2 \paren {\frac 1 A - \frac 1 B}}
\end{flalign*}

\begin{code}
    tA = sqrt (2*b*x / (a*b - a**2))
    tC = (x - xmax) / vmax veh

\end{code}

\begin{code}
timeToTravel veh x = tA + tB + tC
  where (tA, tB, tC) = timeTriple veh x

\end{code}

\paragraph{Example} Here I have modelled a vehicle that is able to do
$0-60$ mph in $10$ seconds, but has a maximum speed of $25$ mph.  The
braking deceleration is equivalent to one-tenth gravity.  Under these
conditions, it takes approximately $3607.78$ seconds to travel $25$
miles.  This is slightly longer than an hour because of acceleration
and braking time.  As the result of {\tt timeTriple} shows, braking
takes almost three times as long as acceleration.  The cruising time
is under an hour because some of the distance is accounted for in the
acceleration and braking portions of the trip.

\begin{code}
data VTest = V (Double, Double, Double)
instance Vehicle VTest where
  vehicleAccel (V (a,b,v)) = a
  vehicleBrake (V (a,b,v)) = b
  vehicleMaxSpeed (V (a,b,v)) = v

mi2km = (* 1.609344)
kph2mps = (* 1000) . (/ 3600)
mph2mps = kph2mps . mi2km

test1 :: VTest
test1 = V (2.68, -0.981, mph2mps 25)

-- timeToTravel test1 (mi2km 25 * 1000) == 3607.7813029652966
-- timeTriple test1 (mi2km 25 * 1000) == (4.17,11.39,3592.22)

\end{code}

\paragraph{Future considerations} Several assumptions were made by
this model that are not entirely realistic: real vehicles do not
accelerate and decelerate uniformly; the effects of atmospheric drag
are excluded; and finally, for reasons of comfort, the driver may not
always choose to accelerate to maximum velocity before cruising.  With
the exception of drag, which requires second-order differential
equations to analyze, these same techniques can be used to model
varying acceleration and cruising velocities.  However, it requires
additional parameters be added, e.g. drag co-efficients, $\Delta
A,\Delta B$ and possibly some function describing the meaning of
``comfortable acceleration'' in relation to distance.  These are much
harder to quantify, and it is not entirely clear that the result will
be much different in most practical examples.

\end{document}
