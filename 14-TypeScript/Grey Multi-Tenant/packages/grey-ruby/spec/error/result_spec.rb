# frozen_string_literal: true

require "spec_helper"

RSpec.describe GreySdk::Error::Result do
  describe ".ok" do
    it "creates a successful result" do
      result = described_class.ok({ id: 1, name: "Test" })

      expect(result.ok?).to be true
      expect(result.err?).to be false
      expect(result.data).to eq({ id: 1, name: "Test" })
      expect(result.error).to be_nil
    end
  end

  describe ".err" do
    it "creates a failed result from GreyError" do
      error = GreySdk::Error::GreyError.unauthorized
      result = described_class.err(error)

      expect(result.ok?).to be false
      expect(result.err?).to be true
      expect(result.data).to be_nil
      expect(result.error).to eq(error)
    end

    it "wraps non-GreyError in unknown error" do
      result = described_class.err("Some error message")

      expect(result.err?).to be true
      expect(result.error.code).to eq(:unknown)
      expect(result.error.message).to eq("Some error message")
    end
  end

  describe "#unwrap!" do
    it "returns data for successful result" do
      result = described_class.ok({ value: 42 })

      expect(result.unwrap!).to eq({ value: 42 })
    end

    it "raises for failed result" do
      error = GreySdk::Error::GreyError.forbidden("Access denied")
      result = described_class.err(error)

      expect { result.unwrap! }.to raise_error(RuntimeError, /Access denied/)
    end
  end

  describe "#unwrap_or" do
    it "returns data for successful result" do
      result = described_class.ok(10)

      expect(result.unwrap_or(0)).to eq(10)
    end

    it "returns default for failed result" do
      result = described_class.err(GreySdk::Error::GreyError.unknown)

      expect(result.unwrap_or(0)).to eq(0)
    end
  end

  describe "#map" do
    it "transforms data for successful result" do
      result = described_class.ok(5)
      mapped = result.map { |n| n * 2 }

      expect(mapped.ok?).to be true
      expect(mapped.data).to eq(10)
    end

    it "passes through for failed result" do
      error = GreySdk::Error::GreyError.not_found
      result = described_class.err(error)
      mapped = result.map { |n| n * 2 }

      expect(mapped.err?).to be true
      expect(mapped.error).to eq(error)
    end

    it "catches exceptions in transform" do
      result = described_class.ok(5)
      mapped = result.map { |_| raise "Oops" }

      expect(mapped.err?).to be true
      expect(mapped.error.message).to eq("Oops")
    end
  end

  describe "#then" do
    it "chains operations for successful result" do
      result = described_class.ok(5)
      chained = result.then { |n| described_class.ok(n * 2) }

      expect(chained.ok?).to be true
      expect(chained.data).to eq(10)
    end

    it "passes through for failed result" do
      error = GreySdk::Error::GreyError.forbidden
      result = described_class.err(error)
      chained = result.then { |n| described_class.ok(n * 2) }

      expect(chained.err?).to be true
      expect(chained.error).to eq(error)
    end
  end

  describe "#catch" do
    it "handles error for failed result" do
      error = GreySdk::Error::GreyError.not_found
      result = described_class.err(error)
      recovered = result.catch { |_| described_class.ok("default") }

      expect(recovered.ok?).to be true
      expect(recovered.data).to eq("default")
    end

    it "passes through for successful result" do
      result = described_class.ok("value")
      recovered = result.catch { |_| described_class.ok("default") }

      expect(recovered.ok?).to be true
      expect(recovered.data).to eq("value")
    end
  end

  describe "#to_h" do
    it "returns hash for successful result" do
      result = described_class.ok({ id: 1 })

      expect(result.to_h).to eq({ ok: true, data: { id: 1 } })
    end

    it "returns hash for failed result" do
      error = GreySdk::Error::GreyError.unauthorized
      result = described_class.err(error)
      hash = result.to_h

      expect(hash[:ok]).to be false
      expect(hash[:error][:code]).to eq(:unauthorized)
    end
  end
end
