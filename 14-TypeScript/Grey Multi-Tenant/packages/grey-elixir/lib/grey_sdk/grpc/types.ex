defmodule GreySdk.Grpc.Types do
  @moduledoc """
  Data types for gRPC services.
  """

  @typedoc """
  Authentication response data.
  """
  @type auth_data :: %{
          access_token: String.t(),
          refresh_token: String.t(),
          expires_in: non_neg_integer()
        }

  @typedoc """
  User data.
  """
  @type user :: %{
          id: String.t(),
          email: String.t(),
          name: String.t(),
          avatar: String.t() | nil,
          metadata: map() | nil
        }

  @typedoc """
  Project data.
  """
  @type project :: %{
          id: String.t(),
          name: String.t(),
          description: String.t() | nil,
          created_at: String.t() | nil,
          updated_at: String.t() | nil,
          metadata: map() | nil
        }

  @typedoc """
  Projects list response.
  """
  @type projects_data :: %{
          projects: list(project()),
          total: non_neg_integer(),
          page: non_neg_integer(),
          page_size: non_neg_integer()
        }

  @typedoc """
  Query response data.
  """
  @type query_data :: %{
          data: any(),
          metadata: map() | nil
        }

  @typedoc """
  Mutation response data.
  """
  @type mutation_data :: %{
          success: boolean(),
          data: any() | nil,
          metadata: map() | nil
        }

  @doc """
  Creates an AuthData struct.
  """
  @spec auth_data(String.t(), String.t(), non_neg_integer()) :: auth_data()
  def auth_data(access_token, refresh_token, expires_in) do
    %{
      access_token: access_token,
      refresh_token: refresh_token,
      expires_in: expires_in
    }
  end

  @doc """
  Creates a User struct.
  """
  @spec user(String.t(), String.t(), String.t(), keyword()) :: user()
  def user(id, email, name, opts \\ []) do
    %{
      id: id,
      email: email,
      name: name,
      avatar: Keyword.get(opts, :avatar),
      metadata: Keyword.get(opts, :metadata)
    }
  end

  @doc """
  Creates a Project struct.
  """
  @spec project(String.t(), String.t(), keyword()) :: project()
  def project(id, name, opts \\ []) do
    %{
      id: id,
      name: name,
      description: Keyword.get(opts, :description),
      created_at: Keyword.get(opts, :created_at),
      updated_at: Keyword.get(opts, :updated_at),
      metadata: Keyword.get(opts, :metadata)
    }
  end

  @doc """
  Creates a ProjectsData struct.
  """
  @spec projects_data(list(project()), non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          projects_data()
  def projects_data(projects, total, page, page_size) do
    %{
      projects: projects,
      total: total,
      page: page,
      page_size: page_size
    }
  end

  @doc """
  Creates a QueryData struct.
  """
  @spec query_data(any(), map() | nil) :: query_data()
  def query_data(data, metadata \\ nil) do
    %{
      data: data,
      metadata: metadata
    }
  end

  @doc """
  Creates a MutationData struct.
  """
  @spec mutation_data(boolean(), any(), map() | nil) :: mutation_data()
  def mutation_data(success, data \\ nil, metadata \\ nil) do
    %{
      success: success,
      data: data,
      metadata: metadata
    }
  end
end
