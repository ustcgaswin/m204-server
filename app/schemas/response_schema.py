from pydantic import BaseModel, Field
from typing import TypeVar, Generic, Optional, List

DataType = TypeVar('DataType')

class Response(BaseModel, Generic[DataType]):
    message: str
    data: Optional[DataType] = None

class ListResponse(BaseModel, Generic[DataType]):
    message: str
    data: Optional[List[DataType]] = None
    total: Optional[int] = None # Optional: for list endpoints, if you want to include total count
    skip: Optional[int] = None
    limit: Optional[int] = None