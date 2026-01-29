defmodule GreySdk.Grpc.ProjectsService do
  @moduledoc """
  gRPC service for project operations.

  This is a placeholder stub. In a real implementation, this would
  use generated protobuf client stubs from .proto files.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.Types

  @doc """
  Lists projects via gRPC.
  """
  @spec list_projects(GenServer.server(), keyword()) ::
          {:ok, Types.projects_data()} | {:error, term()}
  def list_projects(channel_server, opts \\ []) do
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)

    simulate_authenticated_grpc_call(channel_server, "projects.ListProjects", fn ->
      projects = [
        Types.project("project_1", "Stub Project 1", description: "A stub project for testing"),
        Types.project("project_2", "Stub Project 2")
      ]

      Types.projects_data(projects, 2, page, page_size)
    end)
  end

  @doc """
  Creates a project via gRPC.
  """
  @spec create_project(GenServer.server(), String.t(), keyword()) ::
          {:ok, Types.project()} | {:error, term()}
  def create_project(channel_server, name, opts \\ []) do
    description = Keyword.get(opts, :description)

    simulate_authenticated_grpc_call(channel_server, "projects.CreateProject", fn ->
      Types.project(
        "new_project_id",
        name,
        description: description,
        created_at: DateTime.utc_now() |> DateTime.to_iso8601()
      )
    end)
  end

  # Simulates an authenticated gRPC call
  defp simulate_authenticated_grpc_call(channel_server, _method, result_fn) do
    if Channel.authenticated?(channel_server) do
      with {:ok, _channel} <- Channel.get_channel(channel_server) do
        Process.sleep(1)
        {:ok, result_fn.()}
      end
    else
      {:error, Error.unauthorized()}
    end
  end
end
