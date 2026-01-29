' Grey SDK for Visual Basic - Module Entry Point
' Convenience module for quick SDK access.

Imports Grey.Config
Imports Grey.Domain
Imports Grey.Errors

Namespace Grey

    ''' <summary>
    ''' Module providing static access to SDK creation methods.
    ''' </summary>
    Public Module GreySdk

        ''' <summary>
        ''' Creates a client for local development.
        ''' </summary>
        ''' <param name="port">Optional port number (default: 8080).</param>
        ''' <returns>Configured GreyClient instance.</returns>
        Public Function CreateLocal(Optional port As Integer = 8080) As GreyClient
            Return GreyClient.Local(port)
        End Function

        ''' <summary>
        ''' Creates a client for production use.
        ''' </summary>
        ''' <param name="host">The production API host.</param>
        ''' <param name="port">Optional port number (default: 443).</param>
        ''' <returns>Configured GreyClient instance.</returns>
        Public Function CreateProduction(host As String, Optional port As Integer = 443) As GreyClient
            Return GreyClient.Production(host, port)
        End Function

        ''' <summary>
        ''' Creates a client with custom options.
        ''' </summary>
        ''' <param name="options">Configuration options.</param>
        ''' <returns>Configured GreyClient instance.</returns>
        Public Function Create(options As Options) As GreyClient
            Return New GreyClient(options)
        End Function

        ''' <summary>
        ''' SDK version number.
        ''' </summary>
        Public ReadOnly Property Version As String = "1.0.0"

    End Module

End Namespace
