## Stringlist methods syntax

The syntax for each of the Stringlist class methods is shown below.

Square brackets indicate optional elements of the method invocation. See also the notation conventions for methods and the List of Stringlist methods.

* `[number] sl:Add(itemList)`
* `[%number =] sl:AddImage[ ( [imageName])`
* `[%number =] sl:AddOrdered(string)`
* `[%number =] sl:AddOrderedUnique(string)`
* `[number =] sl:AddUnique(string)`
* `[number] sl:AddUniqueOrdered(string)`
* `[%rc=] sl: AppendCCATempAllocator Info`
* `[%rc=] sl: AppendCertificateInfo[([certificate])`
* `[%rc=] sl: AppendCertificateRequest([PrivateKey=] string, [[Country=] string], [[State=] string], [[City=] string], [[Organization=] string], [[OrganizationalUnit=] string], [[CommonName=] string], [SignatureAlgorithm digestAlgorithm])`
* `[%rc=] sl: AppendCertificateRequestInfo( certificateRequest)`
* `[%rc=] sl: AppendClientCertificateRequest([PublicKey=] string, [[Country=] string], [[State=] string], [[City=] string], [[Organization=] string], [[OrganizationalUnit=] string], [[CommonName=] string], [[Challenge=] string], [SignatureAlgorithm digestAlgorithm])`
* `[%rc=] sl: AppendCPCommandOutput (command, [BufferSize= number])`
* `s1: AppendEncryptedSecurityData(sl, [Password=] string, [[Identifier=] string])`
* `[count] sl: AppendFieldImages[([Image string], [FirstOccurrence number], [MaxOccurrences number], [Options= string], [NullValue= string])`
* `[count] sl: AppendFieldValues[([NameLength= number], [Fieldname= string], [Options= string])`
* `sl: AppendGeneratedPrivateKey([[Length=] number], [[Exponent=] number], [[Salt=] string])`
* `[%rc =] sl: Append JournalData[([[StartTime=] string], [[EndTime=] string], [[Threads=] string], [[Options=] string], [[Journal=] journal])`
* `[%rc=] sl: AppendOpenProcedure[([numLines], [seqIncrement])`
* `s1:AppendPemData( [Data] string, [label=] string)`
* `[number =] sl: AppendPrivateKeyInfo[([sl], [[Password=] string])`
* `[%rc =] sl: AppendProcedureList (file, [pname], [accnt], [date])`
* `[%rc=] sl: AppendSignedCertificate( [PrivateKey=] string, [Request] string, [[Signer=] string], [[StartDate=] string], [[EndDate=] string], [[SerialNumber=] number], [SignatureAlgorithm= digestAlgorithm])`
* `[%rc=] sl: AppendSignedClientCertificate( [Privatekey=] string, [Request] string, [Signer=] string, [[StartDate=] string], [[EndDate=] string], [[SerialNumber=] number], [SignatureAlgorithm digestAlgorithm])`
* `[%rc=] sl: AppendSirfactCommandOutput(command)`
* `[%rc=] sl: AppendSirfactData(datalist)`
* `[count] sl: AppendTrace[([parms])`
* `s1:AppendWebData[([formFieldName= string], [formFieldOccurrence number], [options= string])`
* `Throws FormFieldNotFound`
* `string sl: BinaryProcedureDecode`
* `s1: BinaryProcedureEncode(string, [base64Flag])`
* `sl: BindImage(imageName)`
* `[%rc=] sl: ChangeItemLength(itemNum, length)`
* `%rc = sl: CheckCertificate([PrivateKey=] stringlist, [[Password=] string])`
* `%rc=s1:CheckCertificateRequest([PrivateKey=] stringlist, [[Password=] string])`
* `[%outList =] sl:Compare(inlist, [syncCount], [pad])`
* `%outList sl: Copy`
* `s1: Copy ImageBinding(boundList)`
* `[count] sl:CopyItems(inList, [startItem], [numItems])`
* `%count = sl:Count`
* `%string = sl:CreateLines[( [delim], [AddTrailingDelimiter= boolean])`
* `%outList = sl: DeepCopy`
* `[string] sl: Dequeue`
* `sl: Empty`
* `[count =] sl: Enqueue(string)`
* `%number = s1:Find(string, [startItem])`
* `number = sl: FindImageItem(imageltem, [searchValue], [startItem], [operator])`
* `number sl:FindImageItemUp(imageltem, [searchValue], [startItem], [operator])`
* `%number sl:FindUp(string, [startItem])`
* `%string = sl:FirstItem`
* `sl:GetImage(itemNum, [imageName])`
* `[count] sl: Insert(itemNum, itemList)`
* `[count] sl: InsertImage(itemNum, [imageName])`
* `string sl:Item(itemNum)`
* `%outList sl:ItemDifferenceList(sli, itemNum, [separator])`
* `%length = sl:ItemLength(itemNum)`
* `%string = sl:LastItem`
* `newList = [%(Stringlist):] List(itemlist)`
* `%count = sl:ListToItem(strList2, itemWum, [separator])`
* `%itemNum = sl:Locate(string, [[Start=] number], [[StartColumn=] number], [[EndColumn=] number], [[CaseIndependent=] number], [[ArbitraryQuote=] number])`
* `%itemNum = sl: LocateUp(string, [[Start=] number], [[StartColumn=] number], [[EndColumn=] number], [[CaseIndependent=] number], [[ArbitraryQuote=] number])`
* `%max= %(Stringlist): MaxItemLength`
* `s1:MoveFromId(listId)`
* `s1:MoveTold(listId)`
* `%s1 = [%(Stringlist):] New[([imageName])`
* `[%rc =] sl:Overlay(itemum, [startCol], string)`
* `s1:OverlayImageItem(itemNum, imageltem, [value])`
* `sl: ParseLines(string, [delims], [StripTrailingNull boolean])`
* `%updList = sl:Patch(patchList, [Options= string])`
* `%updList = sl: PatchLines(baselist, [Options= string])`
* `%updString = sl: PatchString(baseString, [Options= string])`
* `%string = sl: PemToString(label, [Occurrence number])`
* `Throws InvalidPemData`
* `[string =] sl: Pop`
* `[count] sl:Print[([[NunWidth=] number], [[Lenwidth=] number], [[Start=] number], [[MaxItens=] number])`
* `[count] sl: Push(string)`
* `[%rc =] sl: RegexCapture(string, regex, [Options= string], [Status= %output])`
* `:Throws InvalidRegex`
* `[%itemNum =] sl: RegexLocate(regex, [startItem], [Options= string], [Status= %output], [StartCol= number], [EndCol= number])`
* `Throws InvalidRegex`
* `[%itemNum =] sl: RegexLocateUp(regex, [startItem], [Options= string], [Status= %output], [StartCol= number], [EndCol= number])`
* `Throws InvalidRegex`
* `%outString = s1: RegexReplaceCorresponding(inString, replacementList, [Options= string], [Status Koutput])`
* `Throws InvalidRegex`
* `[%number =] sl: RegexSplit(inString, regex, [Options= string], [Status= %output], [Add regexSplitOutputOptions])`
* `Throws InvalidRegex`
* `%subsetList = sl: RegexSubset (regex, [Options= string], [Status= %output], [StartCol= number], [EndCol= number])`
* `Throws InvalidRegex`
* `[count] sl: RemoveItem(itemWm)`
* `[%rc=] sl: Replace(number, string)`
* `[%rc=] sl: ReplaceImage(itemNum, [imageName])`
* `sl:Sort(sortOrder, [pad])`
* `Throws InvalidSortSpecification`
* `XoutList = sl: SortNew(sortOrder, [pad])`
* `Throws InvalidSortSpecification`
* `SoutList = sl:Subset(string, [[StartColumn=] number], [[EndColumn=] number], [[CaseIndependent=] number], [[ArbitraryQuote=] number])`
* `XoutList = sl: SubsetImageItem(imageItem, [searchValue], [operator])`
* `s1: UnbindImage`
* `%outList = sl:Update(updList, [%status])`
