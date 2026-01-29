defmodule GreySdk.Domain.Projects do
  @moduledoc """
  Domain module for project operations.

  Provides project listing and creation functionality.
  """

  alias GreySdk.Error
  alias GreySdk.Grpc.Channel
  alias GreySdk.Grpc.ProjectsService
  alias GreySdk.Grpc.Types

  @doc """
  Lists projects with pagination.

  ## Options

  - `:page` - Page number, 1-indexed (default: 1)
  - `:page_size` - Number of items per page (default: 20)

  ## Returns

  - `{:ok, projects_data}` on success
  - `{:error, %GreySdk.Error{}}` if not authenticated or on failure

  ## Examples

      case GreySdk.Domain.Projects.list_projects(channel, page: 1, page_size: 10) do
        {:ok, %{projects: projects, total: total}} ->
          IO.puts("Found \#{total} projects")
          Enum.each(projects, &IO.puts(&1.name))

        {:error, error} ->
          IO.puts("Error: \#{error.message}")
      end
  """
  @spec list_projects(GenServer.server(), keyword()) ::
          {:ok, Types.projects_data()} | {:error, Error.t()}
  def list_projects(channel_server, opts \\ []) do
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)

    with :ok <- validate_authenticated(channel_server),
         :ok <- validate_pagination(page, page_size),
         {:ok, projects_data} <- ProjectsService.list_projects(channel_server, opts) do
      {:ok, projects_data}
    else
      {:error, %Error{} = error} -> {:error, error}
      {:error, reason} -> {:error, Error.from_any(reason)}
    end
  end

  @doc """
  Creates a new project.

  ## Parameters

  - `channel_server` - The channel GenServer pid
  - `name` - Project name (required)
  - `opts` - Additional options
    - `:description` - Optional project description

  ## Returns

  - `{:ok, project}` on success
  - `{:error, %GreySdk.Error{}}` if not authenticated or on failure

  ## Examples

      case GreySdk.Domain.Projects.create_project(channel, "My Project", description: "A new project") do
        {:ok, project} ->
          IO.puts("Created project: \#{project.id}")

        {:error, %GreySdk.Error{code: "VALIDATION_ERROR"} = error} ->
          IO.puts("Invalid input: \#{error.message}")

        {:error, error} ->
          IO.puts("Error: \#{error.message}")
      end
  """
  @spec create_project(GenServer.server(), String.t(), keyword()) ::
          {:ok, Types.project()} | {:error, Error.t()}
  def create_project(channel_server, name, opts \\ []) do
    with :ok <- validate_authenticated(channel_server),
         :ok <- validate_project_name(name),
         {:ok, project} <- ProjectsService.create_project(channel_server, name, opts) do
      {:ok, project}
    else
      {:error, %Error{} = error} -> {:error, error}
      {:error, reason} -> {:error, Error.from_any(reason)}
    end
  end

  # Validation helpers

  defp validate_authenticated(channel_server) do
    if Channel.authenticated?(channel_server) do
      :ok
    else
      {:error, Error.unauthorized()}
    end
  end

  defp validate_pagination(page, _page_size) when not is_integer(page) or page < 1 do
    {:error, Error.validation("Page must be a positive integer")}
  end

  defp validate_pagination(_page, page_size) when not is_integer(page_size) or page_size < 1 do
    {:error, Error.validation("Page size must be a positive integer")}
  end

  defp validate_pagination(_page, page_size) when page_size > 100 do
    {:error, Error.validation("Page size must be <= 100")}
  end

  defp validate_pagination(_page, _page_size), do: :ok

  defp validate_project_name(nil), do: {:error, Error.validation("Project name is required")}
  defp validate_project_name(""), do: {:error, Error.validation("Project name is required")}

  defp validate_project_name(name) when is_binary(name) and byte_size(name) > 255 do
    {:error, Error.validation("Project name must be <= 255 characters")}
  end

  defp validate_project_name(name) when is_binary(name), do: :ok
  defp validate_project_name(_), do: {:error, Error.validation("Project name must be a string")}
end
