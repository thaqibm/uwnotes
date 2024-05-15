#import "typst/cheat_sheet.typ": *
#import "thm.typ": *
#show: thmrules

#let theorem = thmbox("theorem", "Theorem", fill: rgb("#eeffee")).with(numbering: none)
#let corollary = thmplain(
  "corollary",
  "Corollary",
  base: "theorem",
  titlefmt: strong
)
#let definition = thmbox("definition", "Definition", fill: rgb("#ffefd6")).with(numbering: none)

#let example = thmplain("example", "Example").with(numbering: none)
#let lemma = thmbox("lemma", "Lemma", fill: rgb("#ffe6e6")).with(numbering: none)
#let proposition = thmbox("proposition", "Proposition", fill: rgb("#eeffee")).with(numbering: none)
#let algo = thmbox("algorithm", "Algorithm", fill: rgb("#e6f7ff")).with(numbering: none)
#let proof = thmplain(
  "proof",
  "Proof",
  base: "theorem",
  bodyfmt: body => [#body #h(1fr) $square$]
).with(numbering: none)


#show: project.with(
  title: "CS476",
  authors: (
    "",
  ),
)

== Basic Statistics
$
script("Var"(a X + b Y) = a^2 "Var"(X) + b^2 "Var"(Y) + 2a b "Cov"(X,Y)) \
script("Var"(a X - b Y) = a^2 "Var"(X) + b^2 "Var"(Y) - 2a b "Cov"(X,Y)) \
$

=== European Put/Call 
$
C_T = max {S_T - K, 0} \
P_T = max {K - S_T, 0}
$

*Risk free asset*
$
d B(tau) = r dot B(tau) d tau \
B(t) = e^(-r (T - t)) B(T)
$

=== Binomial model
$
S_T("up") = u S_0 space, space S_T("down") = d S_0 \
0 < d < 1 + r < u space space ("Arbitrage free")
$

*Trading Strategy* is a pair $Pi = {delta_0, eta_0}$ representing 
number of stocks and risk-free bonds. Value at $t=0$ and $t=T$ is:
$
Pi_0 = S_0 delta_0 + eta_0 B_0 \
Pi_T = S_T delta_0 + eta_0 B_T
$

An arbitrage strategy is a trading strategy: 
$
Pi_0 = 0 " and " Pi_T > 0 "almost surely" \
Pi_0 < 0 " and " Pi_T >= 0 "almost surely"
$

*Pricing using replicating portfolio*:
Given arbitrage free market
$
"if" Pi_T = V_T "almost surely" arrow.double.r Pi_0 = V_0
$
in general $Pi_t = V_t "for" t in [0,T]$

*Replicating portfolio* period-1 Binomial model
$
&delta_0 S_T^u + eta_0 B_T = V_T^u \
&delta_0 S_T^d + eta_0 B_T = V_T^d \
&=> delta_0 = (V_T^u - V_T^d)/(S_T^u - S_T^d)
$


#theorem("Put-Call Parity")[
Assume an arbitrage free market with risk free interest rate $r >= 0$.
Assume $S_t$ doesn't pay dividends. Then at any time $t in [0, T]$, then
European call $C_t$ and put $P_t$ with same strike price $K$ and same expiry $T$
satisfy
$
C_t - P_t = S_t - K e^(-r (T - t))
$
]

#lemma("Risk neutral expected values")[
  Consider an arbitrage free market
  $
  q^u = (e^(r T) - d)/(u - d)
  $
  and $q^u$ satisfy $q^u in (0,1)$. Then under probability measure $QQ$ with
  $QQ("up") = q^u$ and $QQ("down") = 1 - q^u$ we have 
  $
  S_0 = e^(-r T) EE^QQ (S_T) \
  V_0 = e^(-r T) EE^QQ (V_T)
  $
]

#proposition[
  Under the $N-$period Binomial model with $0 < d < e^(r Delta t) < u$.
  Suppose $V_N$ is a random variable (derivative payout at maturity).
  $
  V_n(omega_1, omega_2, dots, omega_n) = e^(-r Delta t) EE^QQ [V_(n + 1) | cal(F)_(n + 1)] \
  sscript(e^(-r Delta t) (q^u  V_(n+1)(omega_1, omega_2, dots, omega_n, "up") + (1-q^u)  V_(n+1)(omega_1, omega_2, dots, omega_n, "down")))
  $
  Then $V_0$ is the fair value of the derivative at time 0
]

#algo[
  Given $N$, compute $V_0$
  1. Compute all possible payouts $V_T (omega_1, dots, omega_N)$
  2. Go backwards. `For n = N - 1, ..., 0`
    - $"For all states" omega in Omega$
    $ script(V_n (omega_1, dots, omega_N) = e^(-r Delta t) (q^u V_(n+1)("up") + (1 - q^u) V_(n+1)("down"))) $
  3. Return $V_0$
]

#definition("Log Normal Returns")[
  $ X_n = log(S_n)\ 
    Delta X_n = X_(n + 1) - X_n = log(S_(n+1)/S_n)
  $
]


#definition("Standard Brownian Motion")[
  1. $W_0 = 0$ almost surely,
  2. For any $s > t >= 0$ the increment $W_s - W_t$ satisfies 
  $
  W_s - W_t tilde.op N(0, s - t)
  $
  3. For any $0 <= t_1 < t_2 <= t_3 < t_4$ the increments $W_(t_2) - W_(t_1), W_(t_4) - W_(t_3)$ are independent
  4. The sample paths $(t, W_t)$ are continuous almost surely.
]

#definition[
  Let $(W_t)_(t >= 0)$ be a Brownian Motion.
  1. The process
  $ X_t = mu t + sigma W_t, &space& t >= 0 $
  is called a Brownian Motion with drift $mu$ and volatility $sigma$

  2. The process $Y_t = Y_0 exp(X_t)$ is called a geometric Brownian Motion.
]

#definition("Black Scholes Model")[
  Under the Black Scholes Model we assume the random stock price process satisfies

  $
  "d"S_t = S_t dot mu dot d t + S_t dot sigma dot d W_t
  $
]

#proposition("A solution of the BS SDE")[
  $
  "d"S_t = S_t dot mu dot d t + S_t dot sigma dot d W_t
  $
  is given by
  $
  S_t = S_0 exp((mu - sigma^2/2)t + sigma W_t)
  $
]

#definition("Quantile function")[
   $ F^(arrow.l) (y) = inf {x in RR : F(x) >= y }, y in [0,1] $
]

#algo("Inversion method")[
  Given a CDF $F$ sample $X tilde.op F$ as follows
  1. Sample $U tilde.op U(0,1)$
  2. Return $X = F^(arrow.l)(U)$
]

#let colp(x) = text(fill: rgb("#ff1279"), $#x$)
== Monte Carlo estimator
$
&colp(hat(mu)_n^"MC" = 1/n sum_(i=0)^n g(bold(X)_i)) \
&sigma^2 = "Var"(g(bold(X))) arrow.double "Var"(hat(mu)_n^"MC") =  sigma^2/n \
&1 - alpha "CI: " hat(mu)_n^"MC" plus.minus Z_(1 - alpha/2) sigma/sqrt(n) \
&1 - alpha "CI: (unknown " sigma) " : " hat(mu)_n^"MC" plus.minus Z_(1 - alpha/2) S_n/sqrt(n)
$
Where $S_n$ is sample standard deviation.

== Antithetic Variates
- Replace $n$ independent observations with $n/2$ pairs of antithetic observations.

- $(g(bold(X_i)) + g(bold(tilde(X_i))))/2$

- $X_i, tilde(X_i)$ are negatively correlated.

$
&colp(hat(mu)_n^"AV" = 1/((n/2)) sum_(i=1)^n (g(bold(X_i)) + g(bold(tilde(X_i))))/2) \
&"Var"(hat(mu)_n^"AV") = sigma^2/n + 1/n "Cov"(g(bold(X_i)), g(bold(tilde(X_i)))) \
&= "Var"(hat(mu)_n^"MC") (1 + "Cor"(g(bold(X_i)) + g(bold(tilde(X_i))))) \
\
&inline("Var"(hat(mu)_n^"AV") <=   "Var"(hat(mu)_n^"MC")  <=>  "Cor"(g(bold(X_i)) + g(bold(tilde(X_i)))) <= 0) \
\
&"res"_i = (g(bold(X_i)) + g(bold(tilde(X_i))))/2 \
&1-alpha "CI: " hat(mu)_n^"AV" plus.minus Z_(1 - alpha/2) "sd"("res")/sqrt(n)
$
== Control Variates
$ 
colp(hat(mu)_n^"CV" = 1/n sum_(i=1)^n (Y_i + beta (mu_c - C_i))) \
"res"_i = Y_i + beta (mu_c - C_i) \
(1 - alpha) "CI ": hat(mu)_n^"CV" plus.minus Z_(1 - alpha/2) "sd"("res")/sqrt(n)\
EE(C) = mu_C " is known" \
"Var"(hat(mu)_n^"CV") " is minimized when " \
  beta^*  = "Cov"(Y,C)/"Var"(C)
$
To estimate $beta^*$ we can:
1. Use the same sample, resulting estimate $hat(mu)_n^"CV"$ not necessarily unbiased. For large $n$ bias is negligible.
2. Use pilot study. For $n^"pilot"$ sample $(Y_i,C_i)$ and estimate $hat(beta)^*$

- The more correlated $C$ and $Y$ are, the better the improvement over crude MC.
- $hat(mu)_n^"CV"$ is  asymptotically normal so CI can be estimated in normal way.

== Brownian Bridge
- We can sample the path out of order using conditional distributions. This saves time since we may not need to sample the full path

#theorem("BM Conditional Distribution")[
  If $(W_t)_(t >= 0)$ is a standard BM, then for any $u < v < w$ we have
  $
  X = W_v | (W_u = a, W_v = b) \
  X tilde.op N((w - v)/(w - u)a + (v - u)/(w - u)b, ((v-u)(w-v))/(w - u))
  $
]
- We can sample the stock price $S_(t_1), dots, S_(t_N)$ in any order using the conditional distributions.

== Multivariate Normal Distribution
#definition("Multivariate Normal")[
  $
  bold(X) tilde.op N_d (mu, Sigma)
  $
  if $bold(X) = mu + bold(A)Z$, where $Z = (z_1, z_2, dots, z_d)$ with $Z_j tilde.op N(0,1)$
  and $A A^T = Sigma$
]
*Sampling from the multivariate normal*
- Let $A$ be the e Cholesky factor of $Sigma$, which is a lower triangular
matrix so that $A A^T = Sigma$

1. For $i = 1 dots n$
  -  Sample $Z_1, dots, Z_d tilde.op N(0,1)$
  - $bold(X_i) = mu + A Z$
2. Return $bold(X)$

*Correlated assets* 

- Let $Z_1, Z_2 tilde.op N(0,1), Z = (Z_1, Z_2)$ 

- $A = mat(1, 0; rho, sqrt(1 - rho^2))$ and $rho in (-1, 1)$

- $bold(X) = A Z$, then

- $X_1 = Z_1 + 0 dot Z_2$, $X_2 = rho Z_1 + sqrt(1 - rho^2)Z_2$

- $Sigma = mat(1, rho; rho, 1;)$

- $"Cov"(X_1, X_2) = "Cor"(X_1, X_2) = rho$

We can sample two correlated standard Brownian Motions $W^((1)), W^((2))$, $"Cor"(W^((1)), W^((2))) = rho$

1. Sample $Z_1, Z_2 tilde.op N(0,1)$

2. Set $W^((1))_(t_j) = W^((1))_(t_(j-1)) + sqrt(Delta t) Z_1$

3. Set $W^((2))_(t_j) = W^((2))_(t_(j-1)) + sqrt(Delta t) (rho Z_1 + sqrt(1 - rho^2)Z_2)$

== Common Random numbers
- Estimating $mu_1 - mu_2 = EE(g_1(bold(X))) - EE(g_2(bold(X)))$

1. *Method 1*: Estimate $mu_1, mu_2$ using two independent MC estimators $mu_(n, 1)^"MC", mu_(n,2)^"MC"$
$ 
"Var"(mu_n^"MC") = 1/n (sigma_1^2 + sigma_2^2)
$

2. *Method 2* Estimate using the _same_ random numbers $mu_n^"CRN"$
$ 
&"Var"(mu_n^"CRN") = 1/n (sigma_1^2 + sigma_2^2 - 2 sigma_(1,2))\
&sigma_1 = "Var"(g_1(bold(U))) space, space \  
&sigma_2 = "Var"(g_2(bold(U))) space, space \ 
&sigma_(1,2) = "Cov"(g_1(bold(U)), g_2(bold(U)))
$
- CRN  outperform the independent estimator $<==> sigma_(1,2) > 0$ 

= Lebesgue Integral

$
F V(G) = lim_(||Pi|| -> 0) sum_(j = 0)^(n - 1)  |G(t_(j + 1)) - G(t_j)|
$

#definition("Lebesgue Stieltjes Integral")[
If $F V (G) < infinity$ then

$
script(integral_0^T f(t) d G(t) = lim_(n -> infinity) sum_(j = 0)^(n - 1) f(t_j)(G(t_(j + 1)) - G(t_j)))
$

If $G$ has a first derivative $G' = g(t)$ then
$
script(integral_0^T f(t) d G(t)  = integral_0^T f(t)g(t) d t)
$
]

#definition("Expected Value")[
  Suppose $F$ is a distribution function of some r.v $X$ then
  $
    EE(X) = integral x d F(X)
  $
]

#definition("Quadratic Variation")[
$
[f, f] (T) = lim_(||Pi|| -> 0) sum_(j = 0)^(n - 1)  [f(t_(j + 1)) - f(t_j)]^2
$
]

#proposition[
  If $f$ has a continuous first derivative then by Taylor $[f, f](T) = 0$
]

#theorem[
  The Brownian Motion $W(t)$ for $f in [0, T]$ does not have finite first order
variation almost surely

$
  F V(W) = infinity \
  [W, W](T) = T
$
]

#theorem("Ito Formula for a BM")[
  Let $W(t)$ be a BM and $f(t, w)$ be a function for which the partial derivatives
  $f_t, f_w(t, w), f_(w w)(t, w)$ defined and continuous. Then for any $T >= 0$
  $
    &f(T, W(T)) = f(0, W(0)) + integral_0^T f_t (t, W(t)) d t \ 
      &+ integral_0^T f_w (t, W(t)) d W(t)
      + 1/2 integral_0^T f_(w w) (t, W(t)) d t
  $
]

#definition("Ito Process")[
  $
  X(t) = X(0) + integral_0^t a(u) d u + integral_0^t b(u) d W(u)
  $
  also
  $
  d X(t) = a(t)d t + b(t) d W(t)
  $
]

#theorem("Ito’s formula for Ito processes")[
  Let $X(t)$ be an ito process, (suppose all appearing derivatives are continuous) then
  $
  script(&f(T, X(T)) = f(0, X(0)) + integral_0^T f_t (t, X(t)) d t + \
    &integral_0^T f_x (t, X(t)) d X(t) + 1/2 integral_0^t f_(x x)(t, X(t)) d [X, X](t))
  $
  After simplification:

  $
  script(d f(t, X(t)) = (f_t + a(t)f_x + 1/2 f_(x x) b^2(t)) d t + f_x b(t) d W(t))
  $
]

#definition("Euler Approximation")[
$
d X(t) = a(X(t))d t + b(X(t)) d W(t)
$

$hat(X)(0) = X(0)$ and
$
script(hat(X)(t_(i + 1)) = hat(X)(t_i) + a(hat(X)(t_i)) dot Delta + b( hat(X)(t_i) ) dot sqrt(Delta) dot Z_(i + 1))
$
Where $Z_1, Z_2, dots, Z_N tilde.op N(0, 1)$ iid.
]

#definition("Value At Risk")[
  $
  PP(X <= "VaR"_beta(X)) = integral_(- infinity)^("VaR"_beta(X)) f(x) d x = 1 - beta
  $
  Value-at-risk is the $1 - beta$ quantile of the distribution.
]

#definition("Expected Shortfall")[
  $
  "ES"_beta (X) = EE(X | X <= "VaR"_beta) \
  "ES"_beta (X) = 1/(1 - beta) integral_beta^1 "VaR"_u d u
  $

  Note: The expected shortfall is sub-additive
  $
  "ES"_beta (X + Y) <= "ES"_beta (X) + "ES"_beta (Y)
  $
]


#algo("VaR and ES from sample")[
  $X_1, X_2, ..., X_n$ is the sample data.

  - Let $X_((1)) < X_((2)) < ... < X_((n))$ be the sorted data.
  - $i_beta = floor((1 - beta) n)$
  - $hat("VaR")_beta = X_((i_beta))$
  - $hat("ES")_beta = 1/(i_beta) (X_((1)) + X_((2)) + ... + X_((i_beta)))$
]


#definition("Jump proccess")[
  $
  d S_t = mu S_t d t + sigma S_t d W_t + (J - 1) d N(t) 
  $

  Where where $N(t)$ is a poisson process with rate $λ$.
  in a small interval of length $Delta t$ we have

  $
  d N(t) = cases(
    1 "with prob" delta d t \
    0 "with prob" 1 - delta d t
  )
  $
]


