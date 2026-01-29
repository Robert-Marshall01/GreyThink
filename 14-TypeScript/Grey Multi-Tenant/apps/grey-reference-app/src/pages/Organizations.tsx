/**
 * Organizations Page
 * 
 * Manages organizations for multi-tenancy.
 * Allows creating new organizations and selecting the active one.
 */

import { useState, type FormEvent } from 'react';
import { useMutation, useQuery, useGreyContext, type Organization } from '@grey/react';
import { useOrganization } from '../context/OrganizationContext';

/**
 * Organizations Component
 * 
 * Features:
 * - List all organizations the user belongs to
 * - Create new organizations
 * - Select active organization for multi-tenancy
 * 
 * TODO: Add organization settings/edit
 * TODO: Add member management
 * TODO: Add organization deletion with confirmation
 */
export function Organizations() {
  const { baseUrl } = useGreyContext();
  const { activeOrganization, setActiveOrganization } = useOrganization();
  
  // Form state for creating new organization
  const [showCreateForm, setShowCreateForm] = useState(false);
  const [newOrgName, setNewOrgName] = useState('');
  const [newOrgDescription, setNewOrgDescription] = useState('');

  // Fetch organizations using useQuery hook
  // TODO: This would typically use a dedicated useOrganizations hook
  // For now, we'll use a mock/placeholder until the SDK supports listing orgs
  const {
    data: organizations,
    loading: orgsLoading,
    error: orgsError,
    refetch: refetchOrgs,
  } = useQuery<Organization[]>({
    key: 'organizations',
    queryFn: async () => {
      // TODO: Replace with actual SDK call when available
      // return client.organizations.list();
      
      // For now, return empty array or mock data
      // The SDK doesn't have a list organizations endpoint yet
      console.log('Fetching organizations from:', baseUrl);
      
      // Mock implementation - in real app, call:
      // const response = await fetch(`${baseUrl}/organizations`);
      // return response.json();
      
      return [];
    },
    enabled: true,
  });

  // Create organization mutation
  const createOrgMutation = useMutation<Organization, { name: string; description?: string }>({
    mutationFn: async (data) => {
      // TODO: Use the SDK client when available
      // const result = await client.organizations.create(data);
      // return result.data;
      
      const response = await fetch(`${baseUrl}/organizations`, {
        method: 'POST',
        headers: { 
          'Content-Type': 'application/json',
          // Token would be injected by the SDK normally
        },
        body: JSON.stringify(data),
      });
      
      if (!response.ok) {
        throw new Error('Failed to create organization');
      }
      
      const result = await response.json();
      return result.data;
    },
    onSuccess: (newOrg) => {
      // Refetch the list to include new org
      refetchOrgs();
      // Clear form
      setNewOrgName('');
      setNewOrgDescription('');
      setShowCreateForm(false);
      // Set as active organization
      setActiveOrganization(newOrg);
    },
    onError: (error) => {
      console.error('Failed to create organization:', error);
    },
  });

  const handleCreateOrg = (e: FormEvent) => {
    e.preventDefault();
    
    if (!newOrgName.trim()) {
      return;
    }

    createOrgMutation.mutate({
      name: newOrgName.trim(),
      description: newOrgDescription.trim() || undefined,
    });
  };

  const handleSelectOrg = (org: Organization) => {
    setActiveOrganization(org);
  };

  return (
    <div>
      <div style={{ 
        display: 'flex', 
        justifyContent: 'space-between', 
        alignItems: 'center',
        marginBottom: '1.5rem',
      }}>
        <h1>Organizations</h1>
        
        <button
          onClick={() => setShowCreateForm(!showCreateForm)}
          style={{
            padding: '0.5rem 1rem',
            background: showCreateForm ? '#6b7280' : '#1a1a2e',
            color: 'white',
            border: 'none',
            borderRadius: '4px',
            cursor: 'pointer',
          }}
        >
          {showCreateForm ? 'Cancel' : '+ Create Organization'}
        </button>
      </div>

      {/* Active organization indicator */}
      {activeOrganization && (
        <div style={{
          padding: '0.75rem 1rem',
          background: '#dcfce7',
          border: '1px solid #86efac',
          borderRadius: '4px',
          marginBottom: '1rem',
        }}>
          <strong>Active:</strong> {activeOrganization.name}
        </div>
      )}

      {/* Error state */}
      {orgsError && (
        <div style={{
          padding: '1rem',
          background: '#fee2e2',
          border: '1px solid #fecaca',
          borderRadius: '4px',
          color: '#dc2626',
          marginBottom: '1rem',
        }}>
          Error loading organizations: {orgsError.message}
        </div>
      )}

      {/* Create organization form */}
      {showCreateForm && (
        <div style={{
          background: 'white',
          padding: '1.5rem',
          borderRadius: '8px',
          boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
          marginBottom: '1.5rem',
        }}>
          <h2 style={{ fontSize: '1.25rem', marginBottom: '1rem' }}>Create New Organization</h2>
          
          <form onSubmit={handleCreateOrg}>
            <div style={{ marginBottom: '1rem' }}>
              <label 
                htmlFor="orgName"
                style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
              >
                Organization Name *
              </label>
              <input
                id="orgName"
                type="text"
                value={newOrgName}
                onChange={(e) => setNewOrgName(e.target.value)}
                placeholder="My Organization"
                disabled={createOrgMutation.loading}
                style={{
                  width: '100%',
                  padding: '0.75rem',
                  border: '1px solid #ddd',
                  borderRadius: '4px',
                  fontSize: '1rem',
                }}
              />
            </div>

            <div style={{ marginBottom: '1rem' }}>
              <label 
                htmlFor="orgDescription"
                style={{ display: 'block', marginBottom: '0.5rem', fontWeight: 500 }}
              >
                Description (optional)
              </label>
              <textarea
                id="orgDescription"
                value={newOrgDescription}
                onChange={(e) => setNewOrgDescription(e.target.value)}
                placeholder="A brief description of this organization"
                disabled={createOrgMutation.loading}
                rows={3}
                style={{
                  width: '100%',
                  padding: '0.75rem',
                  border: '1px solid #ddd',
                  borderRadius: '4px',
                  fontSize: '1rem',
                  resize: 'vertical',
                }}
              />
            </div>

            {createOrgMutation.error && (
              <div style={{
                padding: '0.75rem',
                background: '#fee2e2',
                border: '1px solid #fecaca',
                borderRadius: '4px',
                color: '#dc2626',
                marginBottom: '1rem',
                fontSize: '0.875rem',
              }}>
                {createOrgMutation.error.message}
              </div>
            )}

            <button
              type="submit"
              disabled={createOrgMutation.loading || !newOrgName.trim()}
              style={{
                padding: '0.75rem 1.5rem',
                background: createOrgMutation.loading ? '#9ca3af' : '#1a1a2e',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: createOrgMutation.loading ? 'not-allowed' : 'pointer',
                fontWeight: 500,
              }}
            >
              {createOrgMutation.loading ? 'Creating...' : 'Create Organization'}
            </button>
          </form>
        </div>
      )}

      {/* Organizations list */}
      <div style={{
        background: 'white',
        borderRadius: '8px',
        boxShadow: '0 1px 3px rgba(0,0,0,0.1)',
        overflow: 'hidden',
      }}>
        {orgsLoading ? (
          <div style={{ padding: '2rem', textAlign: 'center', color: '#666' }}>
            Loading organizations...
          </div>
        ) : organizations && organizations.length > 0 ? (
          <ul style={{ listStyle: 'none', margin: 0, padding: 0 }}>
            {organizations.map((org) => (
              <li 
                key={org.id}
                style={{
                  padding: '1rem 1.5rem',
                  borderBottom: '1px solid #e5e7eb',
                  display: 'flex',
                  justifyContent: 'space-between',
                  alignItems: 'center',
                  background: activeOrganization?.id === org.id ? '#f0fdf4' : 'transparent',
                }}
              >
                <div>
                  <div style={{ fontWeight: 500 }}>{org.name}</div>
                  <div style={{ fontSize: '0.875rem', color: '#666' }}>
                    ID: {org.id}
                  </div>
                </div>

                <button
                  onClick={() => handleSelectOrg(org)}
                  disabled={activeOrganization?.id === org.id}
                  style={{
                    padding: '0.5rem 1rem',
                    background: activeOrganization?.id === org.id ? '#86efac' : '#e5e7eb',
                    color: activeOrganization?.id === org.id ? '#166534' : '#374151',
                    border: 'none',
                    borderRadius: '4px',
                    cursor: activeOrganization?.id === org.id ? 'default' : 'pointer',
                    fontWeight: 500,
                  }}
                >
                  {activeOrganization?.id === org.id ? 'Active' : 'Select'}
                </button>
              </li>
            ))}
          </ul>
        ) : (
          <div style={{ padding: '2rem', textAlign: 'center', color: '#666' }}>
            <p>No organizations found.</p>
            <p style={{ fontSize: '0.875rem', marginTop: '0.5rem' }}>
              Create your first organization to get started.
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
