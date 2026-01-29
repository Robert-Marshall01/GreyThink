defmodule GreySdk.Application do
  @moduledoc """
  OTP Application module for Grey SDK.

  This module is started automatically when the application starts.
  It can be used to start supervised clients from configuration.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = []

    opts = [strategy: :one_for_one, name: GreySdk.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
