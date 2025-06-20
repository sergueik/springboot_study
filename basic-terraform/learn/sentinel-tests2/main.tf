import "tfplan/v2" as tfplan

getArpFlood  =  func(rc) {
  z = rc.values.content.arpFlood else ""
  print ("checking violations of ", z)
  return z
}

bds = filter tfplan.planned_values.resources as _, rc {
    rc.type is "aci_rest_managed" and rc.values.class_name is "fvBD"
}
violations = filter bds as _, rc {

  getArpFlood (rc) is not "yes"
}

has_violations = length(violations) > 0 else true

if has_violations {
  print("violating resource keys:", keys(violations)) 
}

violating_bds = rule when has_violations {
  false
}

main = rule {
    violating_bds
}
