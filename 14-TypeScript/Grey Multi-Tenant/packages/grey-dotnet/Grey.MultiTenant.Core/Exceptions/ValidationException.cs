using System.Net;
using Grey.MultiTenant.Core.Models;

namespace Grey.MultiTenant.Core.Exceptions;

/// <summary>
/// Exception for validation errors.
/// </summary>
public class ValidationException : ApiException
{
    public IReadOnlyList<ValidationError> ValidationErrors { get; }

    public ValidationException(string message)
        : base(message, HttpStatusCode.BadRequest, "VALIDATION_ERROR")
    {
        ValidationErrors = Array.Empty<ValidationError>();
    }

    public ValidationException(string message, IEnumerable<ValidationError> errors)
        : base(message, HttpStatusCode.BadRequest, "VALIDATION_ERROR")
    {
        ValidationErrors = errors.ToList().AsReadOnly();
    }

    public ValidationException(IEnumerable<ValidationError> errors)
        : base("Validation failed", HttpStatusCode.BadRequest, "VALIDATION_ERROR")
    {
        ValidationErrors = errors.ToList().AsReadOnly();
    }

    public ValidationException(
        string message,
        IEnumerable<ValidationError> errors,
        string? rawResponse = null,
        Exception? innerException = null)
        : base(message, HttpStatusCode.BadRequest, "VALIDATION_ERROR", rawResponse, null, innerException)
    {
        ValidationErrors = errors.ToList().AsReadOnly();
    }

    /// <summary>
    /// Gets validation errors for a specific field.
    /// </summary>
    public IEnumerable<ValidationError> GetErrorsForField(string field) =>
        ValidationErrors.Where(e => string.Equals(e.Field, field, StringComparison.OrdinalIgnoreCase));

    /// <summary>
    /// Checks if there are errors for a specific field.
    /// </summary>
    public bool HasErrorForField(string field) =>
        ValidationErrors.Any(e => string.Equals(e.Field, field, StringComparison.OrdinalIgnoreCase));
}
