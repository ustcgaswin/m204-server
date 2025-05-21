## SNAPLIMD parameter

**Summary**

*   Default value: 3
*   Parameter type: System
*   Where set: System manager resettable
*   Related products: All
*   Introduced: Sirius Mods 8.1

**Description**

This parameter limits the number of snaps which can be issued with any given snap title. That is, it prevents some bug from using up the snaps available as specified by the SNAPLIM parameter. The minimum value of SNAPLIMD is 0, which indicates that there is no limit (other than SNAPLIM, of course) to the number of snaps for any given snap title, and the maximum value is 32767. The default value is 3.

**Usage notes**

*   The `*SNAP` snap title is not subject to SNAPLIMD, thus, whenever either the "SNAP" or SIRIUS SNAPLMOD command is issued, each snap thus created is considered unique, and will not be suppressed due to SNAPLIMD.
*   When the BUMPSNAP command is issued, each snap thus created is considered unique, and will not be suppressed due to SNAPLIMD.
*   When the value of SNAPLIMD is greater than 0, the overall count of snaps (that is, the value of the SNAPID parameter) and the count for each snap title reflect snaps that were actually generated; snaps which are suppressed either due to SNAPLIM or SNAPLIMD do not increment those counts.

One useful consequence of this is that, as long as SNAPLIMD had not been zero whenever any snaps occurred, you can be sure that SNAPID will always be less than or equal to the value of SNAPLIM, and so, to increase SNAPLIM to generate an additional snap, you can simply:

```
RESET SNAPLIM 1+
```

Note that if you want to effectively disable the "duplicate elimination" feature of SNAPLIMD (but why would you?) but keep this benefit, you can:

```
R SNAPLIMD 32767
```

(Any smaller big number will have the same effect, and makes no difference.)

When the value of SNAPLIMD is 0, the value of the SNAPID parameter is incremented for each snap whether it is generated or suppressed, but the count for each snap title reflects only snaps that were actually generated.

*   If SNAPLIMD is (and has always been) greater than 0 (and so is used to limit duplicate snaps), to generate additional snaps for those which have already reached the value of SNAPLIMD, you can increase its value, for example:

```
RESET SNAPLIMD 3+
```

In order to generate these additional snaps, of course, you may also need to increase the value of the SNAPLIM parameter.

The RESET examples above use the feature described in Relative values for Model 204 numeric parameters.

The table of snap titles used by the SNAPLIMD parameter can be viewed or re-initialized using the SNAPLIMD command.

**Categories:** System parameters, Parameters
