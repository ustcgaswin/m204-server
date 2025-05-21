## DEFINE DATASET command

**Syntax**

```
DEFINE DATASET dataset-name
```

**Options**

| Option | Meaning |
|---|---|
| `BUIL` or `BUILD` | Assigning a configuration primary access |
| `COPY` or `COPYING` | Specifies that the dataset is a copy of another dataset |
| `DUMPING` | Specifies that the dataset is a dump of another dataset |
| `RANGE` | Specifies the range of blocks to be included in the dataset |
| `BLOCK` | Specifies the block size |
| `TRACK` | Specifies the track size |
| `ZONE` | Specifies the zone size |
| `DATA` | Specifies the data size |
| `ALIAS` | Specifies an alias for the dataset |
| `VOLUME` or `VOL` | Specifies the volume |
| `DEVICE` | Specifies the device |
| `SECURITY` | Specifies the security attributes |
| `DESCRIPTION` | Specifies a description for the dataset |
| `LRECL` | Specifies the logical record length |
| `RECFM` | Specifies the record format |
| `DSORG` | Specifies the dataset organization |
| `SPACE` | Specifies the space allocation |
| `SEQUENTIAL` or `SEQ` | Specifies sequential organization |
| `KEYED` or `K` | Specifies keyed organization |
| `INDEXED` or `I` | Specifies indexed organization |
| `VSAM` | Specifies VSAM organization |
| `RDA` | Specifies the relative data address |
| `BLOCKED` | Specifies blocked organization |
| `UNBLOCKED` | Specifies unblocked organization |
| `RECORD` | Specifies the record length |
| `LABEL` | Specifies the label |
| `VOLUME-SERIAL` | Specifies the volume serial number |
| `ROUND` | Specifies the rounding method |
| `PRIMARY` | Specifies the primary dataset |
| `SECONDARY` | Specifies the secondary dataset |
| `ACCESS` | Specifies the access method |
| `MODE` | Specifies the mode of access |
| `LABEL-TYPE` | Specifies the label type |
| `LABEL-CLASS` | Specifies the label class |
| `LABEL-CLASS-NAME` | Specifies the label class name |
| `LABEL-CLASS-ID` | Specifies the label class ID |
| `LABEL-CLASS-TYPE` | Specifies the label class type |
| `LABEL-CLASS-SIZE` | Specifies the label class size |
| `LABEL-CLASS-FORMAT` | Specifies the label class format |
| `LABEL-CLASS-OPTIONS` | Specifies the label class options |
| `LABEL-CLASS-ATTRIBUTES` | Specifies the label class attributes |
| `LABEL-CLASS-NAME-LENGTH` | Specifies the label class name length |
| `LABEL-CLASS-ID-LENGTH` | Specifies the label class ID length |
| `LABEL-CLASS-TYPE-LENGTH` | Specifies the label class type length |
| `LABEL-CLASS-SIZE-LENGTH` | Specifies the label class size length |
| `LABEL-CLASS-FORMAT-LENGTH` | Specifies the label class format length |
| `LABEL-CLASS-OPTIONS-LENGTH` | Specifies the label class options length |
| `LABEL-CLASS-ATTRIBUTES-LENGTH` | Specifies the label class attributes length |
| `LABEL-CLASS-NAME-OFFSET` | Specifies the label class name offset |
| `LABEL-CLASS-ID-OFFSET` | Specifies the label class ID offset |
| `LABEL-CLASS-TYPE-OFFSET` | Specifies the label class type offset |
| `LABEL-CLASS-SIZE-OFFSET` | Specifies the label class size offset |
| `LABEL-CLASS-FORMAT-OFFSET` | Specifies the label class format offset |
| `LABEL-CLASS-OPTIONS-OFFSET` | Specifies the label class options offset |
| `LABEL-CLASS-ATTRIBUTES-OFFSET` | Specifies the label class attributes offset |
| `LABEL-CLASS-NAME-VALUE` | Specifies the label class name value |
| `LABEL-CLASS-ID-VALUE` | Specifies the label class ID value |
| `LABEL-CLASS-TYPE-VALUE` | Specifies the label class type value |
| `LABEL-CLASS-SIZE-VALUE` | Specifies the label class size value |
| `LABEL-CLASS-FORMAT-VALUE` | Specifies the label class format value |
| `LABEL-CLASS-OPTIONS-VALUE` | Specifies the label class options value |
| `LABEL-CLASS-ATTRIBUTES-VALUE` | Specifies the label class attributes value |
| `LABEL-CLASS-NAME-TYPE` | Specifies the label class name type |
| `LABEL-CLASS-ID-TYPE` | Specifies the label class ID type |
| `LABEL-CLASS-TYPE-TYPE` | Specifies the label class type type |
| `LABEL-CLASS-SIZE-TYPE` | Specifies the label class size type |
| `LABEL-CLASS-FORMAT-TYPE` | Specifies the label class format type |
| `LABEL-CLASS-OPTIONS-TYPE` | Specifies the label class options type |
| `LABEL-CLASS-ATTRIBUTES-TYPE` | Specifies the label class attributes type |
| `LABEL-CLASS-NAME-LENGTH-TYPE` | Specifies the label class name length type |
| `LABEL-CLASS-ID-LENGTH-TYPE` | Specifies the label class ID length type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE` | Specifies the label class type length type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE` | Specifies the label class size length type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE` | Specifies the label class format length type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE` | Specifies the label class options length type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE` | Specifies the label class attributes length type |
| `LABEL-CLASS-NAME-OFFSET-TYPE` | Specifies the label class name offset type |
| `LABEL-CLASS-ID-OFFSET-TYPE` | Specifies the label class ID offset type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE` | Specifies the label class type offset type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE` | Specifies the label class size offset type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE` | Specifies the label class format offset type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE` | Specifies the label class options offset type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE` | Specifies the label class attributes offset type |
| `LABEL-CLASS-NAME-VALUE-TYPE` | Specifies the label class name value type |
| `LABEL-CLASS-ID-VALUE-TYPE` | Specifies the label class ID value type |
| `LABEL-CLASS-TYPE-VALUE-TYPE` | Specifies the label class type value type |
| `LABEL-CLASS-SIZE-VALUE-TYPE` | Specifies the label class size value type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE` | Specifies the label class format value type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE` | Specifies the label class options value type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE` | Specifies the label class attributes value type |
| `LABEL-CLASS-NAME-TYPE-TYPE` | Specifies the label class name type type |
| `LABEL-CLASS-ID-TYPE-TYPE` | Specifies the label class ID type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE` | Specifies the label class type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE` | Specifies the label class size type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE` | Specifies the label class format type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE` | Specifies the label class options type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE` | Specifies the label class attributes type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE` | Specifies the label class name length type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE` | Specifies the label class ID length type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE` | Specifies the label class type length type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE` | Specifies the label class size length type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE` | Specifies the label class format length type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE` | Specifies the label class options length type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE` | Specifies the label class attributes length type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE` | Specifies the label class name offset type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE` | Specifies the label class ID offset type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE` | Specifies the label class type offset type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE` | Specifies the label class size offset type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE` | Specifies the label class format offset type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE` | Specifies the label class options offset type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE` | Specifies the label class attributes offset type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE` | Specifies the label class name value type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE` | Specifies the label class ID value type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE` | Specifies the label class type value type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE` | Specifies the label class size value type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE` | Specifies the label class format value type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE` | Specifies the label class options value type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE` | Specifies the label class attributes value type type |
| `LABEL-CLASS-NAME-TYPE-TYPE-TYPE` | Specifies the label class name type type type |
| `LABEL-CLASS-ID-TYPE-TYPE-TYPE` | Specifies the label class ID type type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE-TYPE` | Specifies the label class size type type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE-TYPE` | Specifies the label class format type type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE-TYPE` | Specifies the label class options type type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE-TYPE` | Specifies the label class attributes type type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class name length type type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class ID length type type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class type length type type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class size length type type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class format length type type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class options length type type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class attributes length type type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class name offset type type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class ID offset type type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class type offset type type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class size offset type type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class format offset type type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class options offset type type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class attributes offset type type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE-TYPE` | Specifies the label class name value type type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE-TYPE` | Specifies the label class ID value type type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE-TYPE` | Specifies the label class type value type type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE-TYPE` | Specifies the label class size value type type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE-TYPE` | Specifies the label class format value type type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE-TYPE` | Specifies the label class options value type type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE-TYPE` | Specifies the label class attributes value type type type |
| `LABEL-CLASS-NAME-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name type type type type |
| `LABEL-CLASS-ID-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID type type type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type type type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size type type type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format type type type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options type type type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes type type type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name length type type type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID length type type type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type length type type type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size length type type type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format length type type type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options length type type type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes length type type type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name offset type type type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID offset type type type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type offset type type type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size offset type type type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format offset type type type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options offset type type type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes offset type type type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name value type type type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID value type type type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type value type type type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size value type type type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format value type type type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options value type type type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes value type type type type |
| `SIZE` | Specifies the size of the dataset |
| `LABEL-TYPE` | Specifies the label type |
| `VOLUME-SERIAL` | Specifies the volume serial number |
| `ROUND` | Specifies the rounding method |
| `PRIMARY` | Specifies the primary dataset |
| `SECONDARY` | Specifies the secondary dataset |
| `ACCESS` | Specifies the access method |
| `MODE` | Specifies the mode of access |
| `LABEL-TYPE` | Specifies the label type |
| `LABEL-CLASS` | Specifies the label class |
| `LABEL-CLASS-NAME` | Specifies the label class name |
| `LABEL-CLASS-ID` | Specifies the label class ID |
| `LABEL-CLASS-TYPE` | Specifies the label class type |
| `LABEL-CLASS-SIZE` | Specifies the label class size |
| `LABEL-CLASS-FORMAT` | Specifies the label class format |
| `LABEL-CLASS-OPTIONS` | Specifies the label class options |
| `LABEL-CLASS-ATTRIBUTES` | Specifies the label class attributes |
| `LABEL-CLASS-NAME-LENGTH` | Specifies the label class name length |
| `LABEL-CLASS-ID-LENGTH` | Specifies the label class ID length |
| `LABEL-CLASS-TYPE-LENGTH` | Specifies the label class type length |
| `LABEL-CLASS-SIZE-LENGTH` | Specifies the label class size length |
| `LABEL-CLASS-FORMAT-LENGTH` | Specifies the label class format length |
| `LABEL-CLASS-OPTIONS-LENGTH` | Specifies the label class options length |
| `LABEL-CLASS-ATTRIBUTES-LENGTH` | Specifies the label class attributes length |
| `LABEL-CLASS-NAME-OFFSET` | Specifies the label class name offset |
| `LABEL-CLASS-ID-OFFSET` | Specifies the label class ID offset |
| `LABEL-CLASS-TYPE-OFFSET` | Specifies the label class type offset |
| `LABEL-CLASS-SIZE-OFFSET` | Specifies the label class size offset |
| `LABEL-CLASS-FORMAT-OFFSET` | Specifies the label class format offset |
| `LABEL-CLASS-OPTIONS-OFFSET` | Specifies the label class options offset |
| `LABEL-CLASS-ATTRIBUTES-OFFSET` | Specifies the label class attributes offset |
| `LABEL-CLASS-NAME-VALUE` | Specifies the label class name value |
| `LABEL-CLASS-ID-VALUE` | Specifies the label class ID value |
| `LABEL-CLASS-TYPE-VALUE` | Specifies the label class type value |
| `LABEL-CLASS-SIZE-VALUE` | Specifies the label class size value |
| `LABEL-CLASS-FORMAT-VALUE` | Specifies the label class format value |
| `LABEL-CLASS-OPTIONS-VALUE` | Specifies the label class options value |
| `LABEL-CLASS-ATTRIBUTES-VALUE` | Specifies the label class attributes value |
| `LABEL-CLASS-NAME-TYPE` | Specifies the label class name type |
| `LABEL-CLASS-ID-TYPE` | Specifies the label class ID type |
| `LABEL-CLASS-TYPE-TYPE` | Specifies the label class type type |
| `LABEL-CLASS-SIZE-TYPE` | Specifies the label class size type |
| `LABEL-CLASS-FORMAT-TYPE` | Specifies the label class format type |
| `LABEL-CLASS-OPTIONS-TYPE` | Specifies the label class options type |
| `LABEL-CLASS-ATTRIBUTES-TYPE` | Specifies the label class attributes type |
| `LABEL-CLASS-NAME-LENGTH-TYPE` | Specifies the label class name length type |
| `LABEL-CLASS-ID-LENGTH-TYPE` | Specifies the label class ID length type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE` | Specifies the label class type length type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE` | Specifies the label class size length type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE` | Specifies the label class format length type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE` | Specifies the label class options length type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE` | Specifies the label class attributes length type |
| `LABEL-CLASS-NAME-OFFSET-TYPE` | Specifies the label class name offset type |
| `LABEL-CLASS-ID-OFFSET-TYPE` | Specifies the label class ID offset type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE` | Specifies the label class type offset type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE` | Specifies the label class size offset type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE` | Specifies the label class format offset type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE` | Specifies the label class options offset type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE` | Specifies the label class attributes offset type |
| `LABEL-CLASS-NAME-VALUE-TYPE` | Specifies the label class name value type |
| `LABEL-CLASS-ID-VALUE-TYPE` | Specifies the label class ID value type |
| `LABEL-CLASS-TYPE-VALUE-TYPE` | Specifies the label class type value type |
| `LABEL-CLASS-SIZE-VALUE-TYPE` | Specifies the label class size value type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE` | Specifies the label class format value type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE` | Specifies the label class options value type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE` | Specifies the label class attributes value type |
| `LABEL-CLASS-NAME-TYPE-TYPE` | Specifies the label class name type type |
| `LABEL-CLASS-ID-TYPE-TYPE` | Specifies the label class ID type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE` | Specifies the label class type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE` | Specifies the label class size type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE` | Specifies the label class format type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE` | Specifies the label class options type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE` | Specifies the label class attributes type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE` | Specifies the label class name length type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE` | Specifies the label class ID length type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE` | Specifies the label class type length type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE` | Specifies the label class size length type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE` | Specifies the label class format length type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE` | Specifies the label class options length type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE` | Specifies the label class attributes length type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE` | Specifies the label class name offset type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE` | Specifies the label class ID offset type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE` | Specifies the label class type offset type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE` | Specifies the label class size offset type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE` | Specifies the label class format offset type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE` | Specifies the label class options offset type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE` | Specifies the label class attributes offset type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE` | Specifies the label class name value type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE` | Specifies the label class ID value type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE` | Specifies the label class type value type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE` | Specifies the label class size value type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE` | Specifies the label class format value type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE` | Specifies the label class options value type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE` | Specifies the label class attributes value type type |
| `LABEL-CLASS-NAME-TYPE-TYPE-TYPE` | Specifies the label class name type type type |
| `LABEL-CLASS-ID-TYPE-TYPE-TYPE` | Specifies the label class ID type type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE-TYPE` | Specifies the label class size type type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE-TYPE` | Specifies the label class format type type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE-TYPE` | Specifies the label class options type type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE-TYPE` | Specifies the label class attributes type type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class name length type type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class ID length type type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class type length type type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class size length type type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class format length type type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class options length type type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE-TYPE` | Specifies the label class attributes length type type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class name offset type type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class ID offset type type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class type offset type type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class size offset type type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class format offset type type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class options offset type type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE-TYPE` | Specifies the label class attributes offset type type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE-TYPE` | Specifies the label class name value type type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE-TYPE` | Specifies the label class ID value type type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE-TYPE` | Specifies the label class type value type type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE-TYPE` | Specifies the label class size value type type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE-TYPE` | Specifies the label class format value type type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE-TYPE` | Specifies the label class options value type type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE-TYPE` | Specifies the label class attributes value type type type |
| `LABEL-CLASS-NAME-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name type type type type |
| `LABEL-CLASS-ID-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID type type type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type type type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size type type type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format type type type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options type type type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes type type type type |
| `LABEL-CLASS-NAME-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name length type type type type |
| `LABEL-CLASS-ID-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID length type type type type |
| `LABEL-CLASS-TYPE-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type length type type type type |
| `LABEL-CLASS-SIZE-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size length type type type type |
| `LABEL-CLASS-FORMAT-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format length type type type type |
| `LABEL-CLASS-OPTIONS-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options length type type type type |
| `LABEL-CLASS-ATTRIBUTES-LENGTH-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes length type type type type |
| `LABEL-CLASS-NAME-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name offset type type type type |
| `LABEL-CLASS-ID-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID offset type type type type |
| `LABEL-CLASS-TYPE-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type offset type type type type |
| `LABEL-CLASS-SIZE-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size offset type type type type |
| `LABEL-CLASS-FORMAT-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format offset type type type type |
| `LABEL-CLASS-OPTIONS-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options offset type type type type |
| `LABEL-CLASS-ATTRIBUTES-OFFSET-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes offset type type type type |
| `LABEL-CLASS-NAME-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name value type type type type |
| `LABEL-CLASS-ID-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID value type type type type |
| `LABEL-CLASS-TYPE-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type value type type type type |
| `LABEL-CLASS-SIZE-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size value type type type type |
| `LABEL-CLASS-FORMAT-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format value type type type type |
| `LABEL-CLASS-OPTIONS-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options value type type type type |
| `LABEL-CLASS-ATTRIBUTES-VALUE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes value type type type type |
| `LABEL-CLASS-NAME-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class name type type type type type |
| `LABEL-CLASS-ID-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class ID type type type type type |
| `LABEL-CLASS-TYPE-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class type type type type type type |
| `LABEL-CLASS-SIZE-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class size type type type type type |
| `LABEL-CLASS-FORMAT-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class format type type type type type |
| `LABEL-CLASS-OPTIONS-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class options type type type type type |
| `LABEL-CLASS-ATTRIBUTES-TYPE-TYPE-TYPE-TYPE-TYPE` | Specifies the label class attributes type type type type type |


**Examples**

*   **Example 1: In-memory file (JCL and CICS)**
*   **Example 2: New data set with a template**
*   **Example 3: Cataloged data sets**
*   **Example 4: Uncataloged data sets**
*   **Example 5: SEQUENTIAL data set**
*   **Example 6: KEYED data set**

**Usage notes**

**Dynamic allocation at CICS**

**External operations**

**Preprocessing data sets**

**Examples**

**Child datasets**

**Publishing before (or after) parameter key**

**Reading specific data sets**

**Notes**