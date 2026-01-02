from mathai import *
def auto_integration(eq):
  count = 3
  while True:
    eq = dowhile(eq, lambda x: integrate_summation(integrate_const(integrate_formula(simplify(expand(x))))))
    out = integrate_clean(eq)
    if "f_integrate" not in str_form(out):
      return dowhile(out, lambda x: simplify(fraction(x)))
    if count > 0:
      eq = integrate_byparts(eq)
      count = count - 1
    else:
      return eq
z =  simplify(parse("1"))
k =  simplify(parse("8987551787"))
m =  simplify(parse("9109383701 * 10^(-40)"))
e1=  simplify(parse("1602176634 * 10^(-28)"))
hbar=simplify(parse("1054571817 * 10^(-43)"))
pi = tree_form("s_pi")
euler = tree_form("s_e")
r = parse("r")
a0 = hbar**2 / (k*e1**2*m)
c2 = z/a0
c1 = (z**3 / (pi * a0**3)).fx("sqrt")
psi = c1 * euler**(-c2 * r)
psi2 = psi**2
laplace_psi = diff(r**2 * diff(psi, r.name), r.name)/r**2
psi2 = simplify(psi2)
integral_psi2 = TreeNode("f_integrate", [psi2 * parse("4")* pi * r**2, r])
integral_psi2 = auto_integration(integral_psi2)
a = limit1(TreeNode("f_limit", [integral_psi2, r]))
b = limit3(limit2(expand(TreeNode("f_limitpinf", [integral_psi2, r]))))
integral_psi2 = simplify(b-a)
V = -(k * z * e1**2)/r
Hpsi = -hbar**2/(2*m) * laplace_psi + V*psi
psiHpsi = psi * Hpsi
integral_psiHpsi = TreeNode("f_integrate", [psiHpsi * parse("4")* pi * r**2, r])
integral_psiHpsi = auto_integration(integral_psiHpsi)
a = limit1(TreeNode("f_limit", [integral_psiHpsi, r]))
b = limit3(limit2(expand(TreeNode("f_limitpinf", [integral_psiHpsi, r]))))
integral_psiHpsi = simplify(b-a)
result =  integral_psiHpsi / integral_psi2
print(compute(result /e1))
