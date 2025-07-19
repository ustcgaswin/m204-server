import asyncio
from app.utils.logger import log

async def async_retry(
    func,
    *args,
    max_attempts: int = 3,
    delay: float = 1.0,
    exceptions: tuple = (Exception,),
    **kwargs
):
    """
    Retries an async function on specified exceptions.

    Args:
        func: The async function to call.
        *args: Positional arguments for func.
        max_attempts: Maximum number of attempts.
        delay: Delay between attempts (seconds).
        exceptions: Tuple of exception classes to catch and retry.
        **kwargs: Keyword arguments for func.

    Returns:
        The result of func(*args, **kwargs) if successful.

    Raises:
        The last exception if all attempts fail.
    """
    attempt = 0
    while attempt < max_attempts:
        try:
            return await func(*args, **kwargs)
        except exceptions as e:
            attempt += 1
            log.warning(f"Attempt {attempt} failed: {e}")
            if attempt >= max_attempts:
                raise
            await asyncio.sleep(delay)