defmodule GreySdk.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/grey/grey-elixir"

  def project do
    [
      app: :grey_sdk,
      version: @version,
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      docs: docs(),
      dialyzer: [
        plt_add_apps: [:mix]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {GreySdk.Application, []}
    ]
  end

  defp deps do
    [
      # gRPC
      {:grpc, "~> 0.7"},
      {:protobuf, "~> 0.12"},

      # HTTP/2 transport
      {:mint, "~> 1.5"},
      {:castore, "~> 1.0"},

      # JSON encoding
      {:jason, "~> 1.4"},

      # Development & Testing
      {:ex_doc, "~> 0.31", only: :dev, runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:mox, "~> 1.1", only: :test}
    ]
  end

  defp description do
    """
    Grey Multi-Tenant SDK for Elixir using gRPC transport.
    """
  end

  defp package do
    [
      name: "grey_sdk",
      licenses: ["MIT"],
      links: %{"GitHub" => @source_url}
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md"],
      source_url: @source_url
    ]
  end
end
