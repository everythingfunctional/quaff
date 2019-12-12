#!/bin/bash

Quantity_module="${1}"
quantitySnake="${2}"
QuantityCamel="${3}"
quantity_lower="${4}"
QUANTITY_CAPITAL="${5}"
units_lower="${6}"
UNITS_CAPITAL="${7}"
unit_sym="${8}"
gnu_sym="${9}"
ltx_sym="${10}"

if [[ $# -ne 10 ]]; then
    echo "Usage:"
    echo "    ${0} Quantity_module quantitySnake QuantityCamel quantity_lower QUANTITY_CAPITAL units_lower UNITS_CAPITAL unit_symbol gnuplot_symbol latex_symbol"
    exit
fi

new_module_name="src/${Quantity_module}_m.f90"
new_type_test_name="tests/${quantity_lower}_type_test.f90"
new_math_test_name="tests/${quantity_lower}_math_operators_test.f90"
new_logical_test_name="tests/${quantity_lower}_logical_operators_test.f90"
new_asserts_name="tests/test_helpers/assertions/${Quantity_module}_asserts_m.f90"

sed "s|Quantity_module|${Quantity_module}|g" "src/Quantity_module_m.f90" \
    | sed "s|quantitySnake|${quantitySnake}|g" \
    | sed "s|QuantityCamel|${QuantityCamel}|g" \
    | sed "s|quantity_lower|${quantity_lower}|g" \
    | sed "s|QUANTITY_CAPITAL|${QUANTITY_CAPITAL}|g" \
    | sed "s|units_lower|${units_lower}|g" \
    | sed "s|UNITS_CAPITAL|${UNITS_CAPITAL}|g" \
    | sed "s|unit_sym|${unit_sym}|g" \
    | sed "s|gnu_sym|${gnu_sym}|g" \
    | sed "s|ltx_sym|${ltx_sym}|g" \
    > "${new_module_name}"

sed "s|Quantity_module|${Quantity_module}|g" "tests/quantity_lower_type_test.f90" \
    | sed "s|quantitySnake|${quantitySnake}|g" \
    | sed "s|QuantityCamel|${QuantityCamel}|g" \
    | sed "s|quantity_lower|${quantity_lower}|g" \
    | sed "s|QUANTITY_CAPITAL|${QUANTITY_CAPITAL}|g" \
    | sed "s|units_lower|${units_lower}|g" \
    | sed "s|UNITS_CAPITAL|${UNITS_CAPITAL}|g" \
    | sed "s|unit_sym|${unit_sym}|g" \
    | sed "s|gnu_sym|${gnu_sym}|g" \
    | sed "s|ltx_sym|${ltx_sym}|g" \
    > "${new_type_test_name}"

sed "s|Quantity_module|${Quantity_module}|g" "tests/quantity_lower_math_operators_test.f90" \
    | sed "s|quantitySnake|${quantitySnake}|g" \
    | sed "s|QuantityCamel|${QuantityCamel}|g" \
    | sed "s|quantity_lower|${quantity_lower}|g" \
    | sed "s|QUANTITY_CAPITAL|${QUANTITY_CAPITAL}|g" \
    | sed "s|units_lower|${units_lower}|g" \
    | sed "s|UNITS_CAPITAL|${UNITS_CAPITAL}|g" \
    | sed "s|unit_sym|${unit_sym}|g" \
    | sed "s|gnu_sym|${gnu_sym}|g" \
    | sed "s|ltx_sym|${ltx_sym}|g" \
    > "${new_math_test_name}"

sed "s|Quantity_module|${Quantity_module}|g" "tests/quantity_lower_logical_operators_test.f90" \
    | sed "s|quantitySnake|${quantitySnake}|g" \
    | sed "s|QuantityCamel|${QuantityCamel}|g" \
    | sed "s|quantity_lower|${quantity_lower}|g" \
    | sed "s|QUANTITY_CAPITAL|${QUANTITY_CAPITAL}|g" \
    | sed "s|units_lower|${units_lower}|g" \
    | sed "s|UNITS_CAPITAL|${UNITS_CAPITAL}|g" \
    | sed "s|unit_sym|${unit_sym}|g" \
    | sed "s|gnu_sym|${gnu_sym}|g" \
    | sed "s|ltx_sym|${ltx_sym}|g" \
    > "${new_logical_test_name}"

sed "s|Quantity_module|${Quantity_module}|g" "tests/test_helpers/assertions/Quantity_module_asserts_m.f90" \
    | sed "s|quantitySnake|${quantitySnake}|g" \
    | sed "s|QuantityCamel|${QuantityCamel}|g" \
    | sed "s|quantity_lower|${quantity_lower}|g" \
    | sed "s|QUANTITY_CAPITAL|${QUANTITY_CAPITAL}|g" \
    | sed "s|units_lower|${units_lower}|g" \
    | sed "s|UNITS_CAPITAL|${UNITS_CAPITAL}|g" \
    | sed "s|unit_sym|${unit_sym}|g" \
    | sed "s|gnu_sym|${gnu_sym}|g" \
    | sed "s|ltx_sym|${ltx_sym}|g" \
    > "${new_asserts_name}"
