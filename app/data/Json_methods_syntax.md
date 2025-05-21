## Json methods syntax

The syntax for each of the Json class methods is shown below.
Square brackets indicate optional elements of the method invocation. See also the notation conventions for methods and the List of Json methods.

* `[%number =] json: Add (json)`
    * Throws `InvalidJsonType`
* `%json = [%(Json): ]Array[( [itemList])]`
* `%json = [%(Json): ] Boolean (boolean)`
* `%boolean = json: BooleanValue`
    * Throws `InvalidJsonType`
* `%outJson = json: Copy`
    * Throws `InvalidJsonType`
* `%count = json: Count`
    * Throws `InvalidJsonType`
* `%outJson = json: DeepCopy`
    * Throws `InvalidJsonType`
* `[%number =] json: Delete(index)`
    * Throws `InvalidJsonType`
* `%json = [%(Json):] False`
    * Throws `InvalidJsonType`
* `[%number =] json: Insert(number, json)`
    * Throws `InvalidJsonType`
* `%currentJson = json:Item(index)`
    * `json: Item(index) = newJson`
    * Throws `InvalidJsonType`
* `%out Json = json: ItemByNumber(number)`
    * Throws `InvalidJsonType`
* `%unicode = json: NameByNumber(number)`
    * Throws `InvalidJsonType`
* `%json = [%(Json):] Number (number)`
* `%number = json: NumberByName(unicode)`
* `%number = json: NumberValue`
    * Throws `InvalidJsonType`
* `%json = [%(Json):]Object`
* `%json = [%(Json):] Parse(unicode)`
    * Throws `JsonParseError`
* `%json = [%(Json):) String (unicode)`
* `%unicode = json: Stringify[( [Indent= number])]`
    * Throws `JsonCircularReference`
* `%unicode = json: StringValue`
    * Throws `InvalidJsonType`
* `%unicode = json: ToString[( [Indent= number])]`
    * Throws `JsonCircularReference`
* `%json = [%(Json):] True`
* `%jsonType = json: Type`
