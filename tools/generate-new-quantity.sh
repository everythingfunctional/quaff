#!/bin/bash

quantity_lower="${1}"
QUANTITY_CAPITAL="${2}"
units_lower="${3}"
UNITS_CAPITAL="${4}"
unit_sym="${5}"

if [[ $# -ne 5 ]]; then
    echo "Usage:"
    echo "    ${0} quantity QUANTITY units UNITS symbol"
    exit
fi

new_module_name="src/${quantity_lower}_m.f90"
new_test_name="test/${quantity_lower}_test.f90"
new_utilities_name="test/utilities/${quantity_lower}_utilities_m.f90"
new_asserts_name="quaff_asserts/src/${quantity_lower}_asserts_m.f90"

sed "s|quantity|${quantity_lower}|g" "tools/templates/quantity_m.f90" \
    | sed "s|QUANTITY|${QUANTITY_CAPITAL}|g" \
    | sed "s|meters|${units_lower}|g" \
    | sed "s|METERS|${UNITS_CAPITAL}|g" \
    | sed "s|SYM|${unit_sym}|g" \
    > "${new_module_name}"

sed "s|quantity|${quantity_lower}|g" "tools/templates/quantity_test.f90" \
    | sed "s|QUANTITY|${QUANTITY_CAPITAL}|g" \
    | sed "s|meters|${units_lower}|g" \
    | sed "s|METERS|${UNITS_CAPITAL}|g" \
    | sed "s|SYM|${unit_sym}|g" \
    > "${new_test_name}"

sed "s|quantity|${quantity_lower}|g" "tools/templates/quantity_utilities_m.f90" \
    | sed "s|QUANTITY|${QUANTITY_CAPITAL}|g" \
    | sed "s|meters|${units_lower}|g" \
    | sed "s|METERS|${UNITS_CAPITAL}|g" \
    | sed "s|SYM|${unit_sym}|g" \
    > "${new_utilities_name}"

sed "s|quantity|${quantity_lower}|g" "tools/templates/quantity_asserts_m.f90" \
    | sed "s|QUANTITY|${QUANTITY_CAPITAL}|g" \
    | sed "s|meters|${units_lower}|g" \
    | sed "s|METERS|${UNITS_CAPITAL}|g" \
    | sed "s|SYM|${unit_sym}|g" \
    > "${new_asserts_name}"
