## Screen classes

Introduced in version 7.1 of the Sirius Mods, Screen objects are designed to make writing and maintaining User Language full-screen applications easier. They also let you take advantage of dynamic 3270 screen sizes in your applications.

Most modern 3270 emulators allow flexible definitions of 3270 screen sizes beyond the standard Models 2, 3, 4, and 5. Traditional User Language screen definitions define a fixed layout for a screen and so must be designed to be display correctly on even the smallest screen. This is usually the 3278 Model 2, which contains a none-too-generous 24 rows by 80 columns. Though a screen designed for a 24x80 layout will usually display correctly on other models, screens designed for other models may display misaligned data on a smaller screen, often rendering the application unusable on these screen types. This leaves the application programmer little choice but to design for the lowest common denominator - Model 2 - wasting precious screen space on larger screens.

With Screen objects, screen definition takes place during User Language request evaluation instead of during compilation, freeing the programmer to detect and take advantage of the current screen size and to make more efficient use of available screen space. It is therefore possible for a single application to support many different screen sizes without changing User Language code or resorting to special functions to manipulate a traditional screen layout.

Screen object sample code and Screen class using object-oriented syntax contain coding examples. If you are a RKTools customer, you can also find screen object coding examples in the SIRIUS file.

**Contents**

1. Screens and ScreenFields
2. System space for Screen objects
3. Useful enumerations
    3.1 ActionKey enumeration
        3.1.1 ToNumber function
        3.1.2 ToNumber syntax
        3.1.3 Syntax terms
    3.2 FieldColor enumeration
    3.3 Highlight enumeration
4. Lists of Screen and ScreenField methods
5. See also

**Screens and ScreenFields**

The Screen class provides an object-oriented (OO) equivalent of the Model 204 full-screen feature. An instance of a Screen object is equivalent to a screen you might define with the SOUL OO full-screen feature. The Screen methods specify layout and certain visual attributes of each screen: where screen items are to appear on the video display, and how they are to be highlighted or colored. Screen objects are composed of screen fields and not screenlines as in the full-screen feature, and multiple screen fields may form the equivalent of a single screenline (or row). These screen fields are themselves objects: instances of the Screenfield class. In this document, ScreenField objects are sometimes referred to as "fields" and sometimes as "screenfields."

**System space for Screen objects**

Traditional User Language full-screen applications use FSCB (Full Screen Buffer) space to hold 3270 screen definitions. But screen objects must fit within the object-oriented architecture, which cannot utilize FSCB space. As a result, screen objects use STBL space for dynamic screen definitions. Because the entire screen image must be in contiguous storage, any instance of a screen object must allocate the maximum allowable STBL space, even if some of this space may not be used by a particular application. You use the SCRNSTBL user parameter, which specifies the maximum size per screen, to prevent unnecessary waste of STBL space and to limit an application to a reasonable amount of space to prevent STBL exhaustion. A good starting value for SCRNSTBL is 6144. Scrolling applications and applications designed for large screens with many fields may require two or more times this storage. An application that exceeds its screen STBL allocation is canceled.

**Useful enumerations**

Screen methods make use of these enumerations:

* ActionKey
* FieldColor
* Highlight

**ActionKey enumeration**

ActionKey is used by the ActionKey and Read methods to specify the keyboard key that activated the last screen read. Valid values are Enter and Pf1, Pf2, and so on through Pf24. The value that is set initially is Enter. As with all enumerations, the ToString method implicitly converts an ActionKey value to a character string whose value is the name of the enumeration value. Other methods available to all enumerations are described in Common enumeration methods. Besides these common methods, the ActionKey enumeration has an additional method available (ToNumber), as described in the following section.

**ToNumber function**

This function converts ActionKey enumeration values to a numeric value: the integer that remains after the key name is stripped of its "PF" prefix. For Enter, the numeric value is 0. These values are useful in a value-testing loop in conjunction with the User Language Jump statement.

**ToNumber syntax**

`%num = key: ToNumber`

**Syntax terms**

* `%num`: A number variable to receive the integer part of the action key name.
* `key`: An ActionKey enumeration variable or expression.

**FieldColor enumeration**

FieldColor is used by the and AddField, Color, ProtectedColor, and UnprotectedColor methods. The valid values (case unimportant) of this enumeration are turquoise, green, blue, red, white, yellow and pink. The default is blue. Note: As with all enumerations, the ToString method implicitly converts a FieldColor value to a character string whose value is the name of the enumeration value. For more information about methods available to all enumerations, see Common enumeration methods.

**Highlight enumeration**

Highlight contains a field's highlight value. It is used by the AddField, Highlight, ProtectedHighlight, and UnprotectedHighlight methods. Valid values of this enumeration are None, Underline, Reverse, and Blink. The default value is None.

**Lists of Screen and ScreenField methods**

The List of Screen methods and the List of ScreenField methods contain complete lists of the methods in these classes, and they are combined in List of Screen API methods.

**See also**

* Screen object sample code and Screen class using object-oriented syntax
* How to use Model 6, or dynamically sized screens, in Model 204
