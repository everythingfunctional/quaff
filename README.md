quaff
=====

[![pipeline status](https://gitlab.com/everythingfunctional/quaff/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/quaff/commits/main)

Quantities for Fortran. Make math with units more convenient.

This library provides all the functionality necessary to *almost* treat
quantities with units associated with them as though they were just intrinsic
real values. However, since a quantity has it's own unique type, you get some
compile time safety that you don't mix them up in your argument lists, and you
don't have to worry about doing unit conversions or remembering what units you've
stored things in when you start doing math with them.

The following gives a basic overview of how you use quaff,
but for more detailed information consult the [developer documentation](https://everythingfunctional.gitlab.io/quaff).

Turning a number into a quantity is as easy as defining what units that number
is in, like `1.0d0.unit.METERS`. And, if you need the number back out, just say
what units you want the value in like `time.in.SECONDS`.

Once you've got your values in quantities, you can do math with them, and not
have to worry about doing unit conversions. All the possible combinations are
appropriately defined. So, assuming these have the types you'd expect, this will
just work: `speed = length / time`.

A variety of `to_string` functions are also provided, so converting to strings
in a variety of formats is easy too. `to_string` will use SI units, and
`to_string_in` allows you to specify the units you'd like. There are also
[quaff_gnuplot_units](https://gitlab.com/everythingfunctional/quaff_gnuplot_units)
and [quaff_latex_units](https://gitlab.com/everythingfunctional/quaff_latex_units)
projects for units that provide formats for those.
You can also specify the number of significant digits you'd like for any of them.

There are `parse_quantity` functions provided for getting a quantity from its
string representation as well. These routines return a `fallible_quantity`
type that may have an `error_list_t`, in case the string could not be properly interpreted.

Finally, all the `assert_equals` functions are provided for working with the
[Vegetables](https://gitlab.com/everythingfunctional/vegetables) unit testing
framework, so you can use quantities in your tests as well.

What If You Don't Have What I Need?
-----------------------------------

### New Units

If a unit you need isn't already defined, you can define your own by simply
defining the conversion factor and strings associated with it. Somewhere, you
just need to have something like the following:

```Fortran
type(length_simple_unit_t), parameter :: CENTIMETERS = &
        length_simple_unit_t( &
                conversion_factor = CENTIMETERS_PER_METER, &
                symbol = "cm")
```

Note, that you'll need to provide an array of possible units you'd like to use
that includes your custom units to any `parse_quantity` functions. Otherwise they
won't know about them. You can change the default output units for any quantity
if you'd like as well, since they aren't defined with the `parameter` attribute.
You could do this in some initialization routine in your code.

### New Quantities

A script is provided that will generate a new quantity for you at
`tools/generate-new-quantity.sh`. It just needs to know the name of the quantity
in lower and upper case, the name of its internal units in lower and upper case,
and the symbol, and it will use the templates to generate the new quantity module,
it's tests, and the assertions. You'll just need to define any additional units
you'd like to use, and the interfaces for the `*` and `/` operators if you'd
like it to do the math correctly with other quantities.

As you can see, if something you need isn't provided, it's highly likely you'll
be able to extend the library without having to actually make any changes to the
upstream code (although pull requests are greatly appreciated :)). See the
[Contributing](CONTRIBUTING.md) file.
