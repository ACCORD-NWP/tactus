"""Define format validators used in the JSON schema validation."""

from datetime import timedelta

import isodate


def duration_format_validator(duration: str) -> bool:
    """Validate the format of a duration string against ISO8601.

    Args:
        duration: The duration string to validate.

    Returns:
        True if the duration string is valid, False otherwise.
    """
    # Try convert duration to timedelta
    try:
        converted = isodate.parse_duration(str(duration))
        # Allow for zero duration
        if converted == timedelta(seconds=0):
            return True
    except ValueError:
        # If not success, let parent know
        return False

    return True


def duration_slice_format_validator(duration_slice: str) -> bool:
    """Validate the format of a duration slice string against ISO8601.

    Each part of the slice is validated using duration_format_validator.

    Args:
        duration_slice: The duration slice string to validate.

    Returns:
        True if the duration slice string is valid, False otherwise.
    """
    splitted = duration_slice.split(":")
    if len(splitted) == 0:
        return False

    result = [duration_format_validator(item) for item in splitted]

    # Check if all items in the result list passed validation
    if all(result) and len(result) <= 3:
        return True
    return False
