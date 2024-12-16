# the circuit families:

each circuit family has its own namespace and can be imported as:

```
import syn::<family>
open syn::<family> -> syn
```
it is imporatant to open the family and eliminate the family prefix to make it generic for the tools.

the family namespaces make it clear if one namespace uses imports from another family.

## bdopt (use with expropt)

## qdi

## qdiopt (use with expropt)

## diopt  (use with expropt)

## ditest  (use with expropt)

for testing purposes, identical to di (loads it), but places a additional non syntesisable buffer in front of every input.

# the control families:

## ring