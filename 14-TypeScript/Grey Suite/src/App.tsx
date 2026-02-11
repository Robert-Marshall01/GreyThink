import { Routes, Route } from 'react-router-dom';
import { Layout } from './components/Layout';
import { Dashboard } from './pages/Dashboard';
import { ERPFinance } from './pages/core/ERPFinance';
import { ERPProcurement } from './pages/core/ERPProcurement';
import { ERPSupplyChain } from './pages/core/ERPSupplyChain';
import { CRMLeads } from './pages/core/CRMLeads';
import { CRMAccounts } from './pages/core/CRMAccounts';
import { CRMOpportunities } from './pages/core/CRMOpportunities';
import { HCMRecruiting } from './pages/core/HCMRecruiting';
import { HCMPayroll } from './pages/core/HCMPayroll';
import { HCMPerformance } from './pages/core/HCMPerformance';
import { Chat } from './pages/collaboration/Chat';
import { VideoConference } from './pages/collaboration/VideoConference';
import { KnowledgeBase } from './pages/collaboration/KnowledgeBase';
import { Whiteboard } from './pages/collaboration/Whiteboard';
import { Transcription } from './pages/collaboration/Transcription';
import { AgentFramework } from './pages/ai/AgentFramework';
import { GenerativeSuite } from './pages/ai/GenerativeSuite';
import { SpecializedAI } from './pages/ai/SpecializedAI';
import { BIDashboards } from './pages/data/BIDashboards';
import { DataWarehouse } from './pages/data/DataWarehouse';
import { ProductAnalytics } from './pages/data/ProductAnalytics';
import { ProjectManagement } from './pages/ops/ProjectManagement';
import { IdentitySecurity } from './pages/ops/IdentitySecurity';
import { FinancePayments } from './pages/ops/FinancePayments';
import { Networking } from './pages/presence/Networking';
import { Reputation } from './pages/presence/Reputation';
import { PersonalCRM } from './pages/presence/PersonalCRM';
import { JobSearch } from './pages/career/JobSearch';
import { SkillDevelopment } from './pages/career/SkillDevelopment';
import { Infrastructure } from './pages/infra/Infrastructure';
import { MultiTenant } from './pages/platform/MultiTenant';
import { UnifiedIdentity } from './pages/platform/UnifiedIdentity';
import { AIOrchestration } from './pages/platform/AIOrchestration';
import { EnterpriseDataWarehouse } from './pages/platform/EnterpriseDataWarehouse';
import { SecurityCompliance } from './pages/platform/SecurityCompliance';
import { IntegrationHub } from './pages/platform/IntegrationHub';
import { WorkflowEngine } from './pages/platform/WorkflowEngine';
import { SimulationEngine } from './pages/platform/SimulationEngine';
import { KnowledgeGraph } from './pages/platform/KnowledgeGraph';
import { RealTimeComms } from './pages/enterprise/RealTimeComms';
import { DocumentManagement } from './pages/enterprise/DocumentManagement';
import { TaskPlanning } from './pages/enterprise/TaskPlanning';
import { CreativeTools } from './pages/enterprise/CreativeTools';
import { DeepAI } from './pages/enterprise/DeepAI';
import { WorkflowAutomation } from './pages/enterprise/WorkflowAutomation';
import { LowCodeBuilder } from './pages/enterprise/LowCodeBuilder';
import { BIPlatform } from './pages/enterprise/BIPlatform';
import { DeviceSecurity } from './pages/enterprise/DeviceSecurity';
import { NetworkProfiles } from './pages/network/NetworkProfiles';
import { NetworkGraph } from './pages/network/NetworkGraph';
import { NetworkFeed } from './pages/network/NetworkFeed';
import { NetworkJobs } from './pages/network/NetworkJobs';
import { NetworkSearch } from './pages/network/NetworkSearch';
import { NetworkTrust } from './pages/network/NetworkTrust';

export default function App() {
    return (
        <Routes>
            <Route path="/" element={<Layout />}>
                <Route index element={<Dashboard />} />
                {/* Core Systems */}
                <Route path="erp/finance" element={<ERPFinance />} />
                <Route path="erp/procurement" element={<ERPProcurement />} />
                <Route path="erp/supply-chain" element={<ERPSupplyChain />} />
                <Route path="crm/leads" element={<CRMLeads />} />
                <Route path="crm/accounts" element={<CRMAccounts />} />
                <Route path="crm/opportunities" element={<CRMOpportunities />} />
                <Route path="hcm/recruiting" element={<HCMRecruiting />} />
                <Route path="hcm/payroll" element={<HCMPayroll />} />
                <Route path="hcm/performance" element={<HCMPerformance />} />
                {/* Collaboration */}
                <Route path="collaboration/chat" element={<Chat />} />
                <Route path="collaboration/video" element={<VideoConference />} />
                <Route path="collaboration/knowledge-base" element={<KnowledgeBase />} />
                <Route path="collaboration/whiteboard" element={<Whiteboard />} />
                <Route path="collaboration/transcription" element={<Transcription />} />
                {/* AI & Automation */}
                <Route path="ai/agent-framework" element={<AgentFramework />} />
                <Route path="ai/generative-suite" element={<GenerativeSuite />} />
                <Route path="ai/specialized" element={<SpecializedAI />} />
                {/* Data Intelligence */}
                <Route path="data/bi" element={<BIDashboards />} />
                <Route path="data/warehouse" element={<DataWarehouse />} />
                <Route path="data/analytics" element={<ProductAnalytics />} />
                {/* Operations */}
                <Route path="ops/projects" element={<ProjectManagement />} />
                <Route path="ops/security" element={<IdentitySecurity />} />
                <Route path="ops/finance" element={<FinancePayments />} />
                {/* Professional Presence */}
                <Route path="presence/networking" element={<Networking />} />
                <Route path="presence/reputation" element={<Reputation />} />
                <Route path="presence/personal-crm" element={<PersonalCRM />} />
                {/* Career */}
                <Route path="career/job-search" element={<JobSearch />} />
                <Route path="career/skills" element={<SkillDevelopment />} />
                {/* Infrastructure */}
                <Route path="infra" element={<Infrastructure />} />
                {/* Platform Engineering */}
                <Route path="platform/multi-tenant" element={<MultiTenant />} />
                <Route path="platform/identity" element={<UnifiedIdentity />} />
                <Route path="platform/ai-orchestration" element={<AIOrchestration />} />
                <Route path="platform/data-warehouse" element={<EnterpriseDataWarehouse />} />
                <Route path="platform/security" element={<SecurityCompliance />} />
                <Route path="platform/integrations" element={<IntegrationHub />} />
                <Route path="platform/workflows" element={<WorkflowEngine />} />
                <Route path="platform/simulations" element={<SimulationEngine />} />
                <Route path="platform/knowledge-graph" element={<KnowledgeGraph />} />
                {/* Enterprise Modules */}
                <Route path="enterprise/realtime-comms" element={<RealTimeComms />} />
                <Route path="enterprise/document-management" element={<DocumentManagement />} />
                <Route path="enterprise/task-planning" element={<TaskPlanning />} />
                <Route path="enterprise/creative-tools" element={<CreativeTools />} />
                <Route path="enterprise/deep-ai" element={<DeepAI />} />
                <Route path="enterprise/workflow-automation" element={<WorkflowAutomation />} />
                <Route path="enterprise/low-code" element={<LowCodeBuilder />} />
                <Route path="enterprise/bi-platform" element={<BIPlatform />} />
                <Route path="enterprise/device-security" element={<DeviceSecurity />} />
                {/* Professional Networking */}
                <Route path="network/profiles" element={<NetworkProfiles />} />
                <Route path="network/graph" element={<NetworkGraph />} />
                <Route path="network/feed" element={<NetworkFeed />} />
                <Route path="network/jobs" element={<NetworkJobs />} />
                <Route path="network/search" element={<NetworkSearch />} />
                <Route path="network/trust" element={<NetworkTrust />} />
            </Route>
        </Routes>
    );
}
