using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace Grey.MultiTenant.Blazor.Stores;

/// <summary>
/// Base class for observable stores with INotifyPropertyChanged support.
/// </summary>
public abstract class StoreBase : INotifyPropertyChanged
{
    public event PropertyChangedEventHandler? PropertyChanged;

    protected void OnPropertyChanged([CallerMemberName] string? propertyName = null)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }

    protected bool SetProperty<T>(ref T field, T value, [CallerMemberName] string? propertyName = null)
    {
        if (EqualityComparer<T>.Default.Equals(field, value))
            return false;

        field = value;
        OnPropertyChanged(propertyName);
        return true;
    }

    /// <summary>
    /// Normalizes an exception into a user-friendly error message.
    /// </summary>
    protected static string NormalizeError(Exception ex)
    {
        return ex switch
        {
            Core.Exceptions.ApiException apiEx => apiEx.Message,
            Core.Exceptions.AuthException authEx => authEx.Message,
            Core.Exceptions.ValidationException valEx => valEx.Message,
            Core.Exceptions.NetworkException netEx => netEx.IsOffline 
                ? "No network connection" 
                : netEx.IsTimeout 
                    ? "Request timed out" 
                    : netEx.Message,
            OperationCanceledException => "Operation was cancelled",
            _ => ex.Message
        };
    }
}
