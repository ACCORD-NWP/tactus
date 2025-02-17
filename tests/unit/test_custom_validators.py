"""Unit tests for the custom validators."""

from unittest.mock import MagicMock, patch

import pytest

from deode.custom_validators import import_from_string


class TestImportFromString:
    """Unit tests for the import_from_string function."""

    @patch("deode.custom_validators.importlib.import_module")
    @patch("deode.custom_validators.sys.modules")
    def test_with_valid_input(self, mock_sys_modules, mock_import_module):
        """Test import_from_string with valid input."""
        mock_module = MagicMock()
        mock_sys_modules.get.return_value = mock_module
        mock_import_module.return_value = mock_module
        mock_module.MyClass = MagicMock()

        result = import_from_string("my_module.MyClass")
        assert result == mock_module.MyClass

    @patch("deode.custom_validators.importlib.import_module")
    @patch("deode.custom_validators.sys.modules")
    def test_with_invalid_module(self, mock_sys_modules, mock_import_module):
        """Test import_from_string with an invalid module."""
        mock_sys_modules.get.return_value = None
        mock_import_module.side_effect = ImportError

        with pytest.raises(ImportError):
            import_from_string("invalid_module.MyClass")

    def test_with_invalid_class(self):
        """Test import_from_string with an invalid class."""
        with pytest.raises(AttributeError):
            import_from_string("deode.custom_validators.MyClass")

    def test_with_empty_string(self):
        """Test import_from_string with an empty string."""
        with pytest.raises(ImportError), patch(
            "deode.custom_validators.importlib.import_module"
        ), patch("deode.custom_validators.sys.modules"):
            import_from_string("")
