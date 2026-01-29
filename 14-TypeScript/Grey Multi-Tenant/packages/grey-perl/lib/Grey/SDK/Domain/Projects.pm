package Grey::SDK::Domain::Projects;

use strict;
use warnings;
use 5.020;

use Grey::SDK::Result;
use Grey::SDK::Error qw(make_error ERROR_VALIDATION);

# Constructor
sub new {
    my ($class, $http_client) = @_;
    
    my $self = {
        http => $http_client,
    };
    
    return bless $self, $class;
}

# List all projects
sub list_projects {
    my ($self, %args) = @_;
    
    my %params;
    $params{page}     = $args{page}     if defined $args{page};
    $params{per_page} = $args{per_page} if defined $args{per_page};
    
    return $self->{http}->get('/api/v1/projects', \%params);
}

# Create a new project
sub create_project {
    my ($self, %args) = @_;
    
    my $name        = $args{name};
    my $description = $args{description};
    my $metadata    = $args{metadata};
    
    return $self->_validation_error('Project name is required')
        unless defined $name && length $name;
    
    my %body = (name => $name);
    $body{description} = $description if defined $description;
    $body{metadata}    = $metadata    if defined $metadata;
    
    return $self->{http}->post('/api/v1/projects', \%body);
}

# Get project by ID
sub get_project {
    my ($self, %args) = @_;
    
    my $project_id = $args{project_id};
    
    return $self->_validation_error('Project ID is required')
        unless defined $project_id && length $project_id;
    
    return $self->{http}->get("/api/v1/projects/$project_id");
}

# Update a project
sub update_project {
    my ($self, %args) = @_;
    
    my $project_id = $args{project_id};
    my $data       = $args{data};
    
    return $self->_validation_error('Project ID is required')
        unless defined $project_id && length $project_id;
    
    return $self->_validation_error('Update data is required')
        unless defined $data && ref($data) eq 'HASH' && %$data;
    
    return $self->{http}->patch("/api/v1/projects/$project_id", $data);
}

# Delete a project
sub delete_project {
    my ($self, %args) = @_;
    
    my $project_id = $args{project_id};
    
    return $self->_validation_error('Project ID is required')
        unless defined $project_id && length $project_id;
    
    return $self->{http}->delete("/api/v1/projects/$project_id");
}

# Create validation error result
sub _validation_error {
    my ($self, $message) = @_;
    
    return Grey::SDK::Result->err(
        make_error(
            code    => ERROR_VALIDATION,
            message => $message,
        )
    );
}

1;

__END__

=head1 NAME

Grey::SDK::Domain::Projects - Projects client for Grey SDK

=head1 SYNOPSIS

    my $projects = Grey::SDK::Domain::Projects->new($http_client);
    
    my $list = $projects->list_projects(page => 1, per_page => 10);
    my $created = $projects->create_project(name => 'New Project');
    my $project = $projects->get_project(project_id => '123');

=cut
